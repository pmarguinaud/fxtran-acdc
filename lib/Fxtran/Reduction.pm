package Fxtran::Reduction;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Reduction

=head1 DESCRIPTION

Rewrites C<ANY>/C<SUM>/C<COUNT> reductions on an array section (bounded by
the routine's own C<KIDIA:KFDIA>-like index pair) into an explicit loop,
without altering the calculations:

  LLDO=ANY (IITER (KIDIA:KFDIA)<IITERMAX)

becomes

  LLDO=.FALSE.
  DO JLON=KIDIA,KFDIA
    LLDO=LLDO .OR. IITER (JLON)<IITERMAX
  ENDDO

Three contexts are handled, in this order:

=over 4

=item * C<DO WHILE (cond)> is first rewritten, generically (regardless of
whether C<cond> mentions a reduction), into C<DO> / C<IF (.NOT. (cond)) THEN>
/ C<EXIT> / C<ENDIF>; any reduction inside C<cond> then falls into the
C<IF>/C<ELSEIF> case below.

=item * A plain assignment C<ACC=ANY (mask)> (or C<SUM>/C<COUNT>) reuses
C<ACC> as the reduction accumulator and replaces the whole statement with
the init+loop.

=item * An C<IF>/C<ELSEIF> chain (any number of branches, each independently
C<ANY>, C<SUM> or C<COUNT>) gets a short-circuiting cascade of reduction
loops hoisted before the C<if-construct>: only the first branch's reduction
is unconditional, each later one is only computed once every earlier
branch's guard has been shown false, mirroring the C<ELSEIF> chain's own
short-circuiting. Each branch's intrinsic call is then replaced by a
reference to its temp.

=back

Every generated reduction C<do-construct> (top-level or nested inside a
cascade guard) is tagged with the attribute
C<reduction="ACC,OP,INIT"> -- e.g. C<reduction="LLANY1,.OR.,.FALSE.">
or C<reduction="ZZSUM1,+,0"> -- where C<ACC> is the accumulator variable it
reduces into (e.g. C<LLDO> for a reused assignment LHS, or a generated
temp), C<OP> is the combining operator (as would appear in e.g. an OpenMP
C<REDUCTION> clause), and C<INIT> is its identity value. C<COUNT>'s
per-element contribution is conditional (see C<%INTRINSIC>), not a direct
C<+>, but the reduction as a whole is still an add-reduction, so it shares
C<SUM>'s C<OP>/C<INIT>.

Scope: only a single array reference with exactly one ranging subscript,
indexed by the exact C<$opts-E<gt>{kidia}>:C<$opts-E<gt>{kfdia}> bounds, is
supported per intrinsic call (any other subscripts, e.g. a fixed C<KLEV>
alongside the ranging one, are left untouched in the generated loop);
anything else (more than one ranging subscript, unrelated bounds, no
reducible reference, a non-reduction condition mixed into an otherwise
all-reduction C<IF>/C<ELSEIF> chain, or a reduction call found outside a
supported context) makes the module C<die> rather than silently produce
incorrect Fortran.

=head1 FUNCTIONS

=cut

use strict;

use XML::LibXML;

use fxtran;
use fxtran::xpath;

use Fxtran;

# Per-intrinsic accumulator type, "op"/init (the reduction operator symbol
# and identity value -- as would appear in e.g. an OpenMP REDUCTION clause;
# COUNT's per-element contribution is conditional, not a direct "+", but the
# reduction as a whole is still an add-reduction, so it uses the same op/init
# as SUM), and the per-iteration combining statement (given the accumulator
# name and the JLON-substituted mask/array-expression text). Naming prefixes
# follow this project's Hungarian-style convention: LL=logical, ZZ=real
# scratch, II=integer scratch.

my %INTRINSIC =
  (
    ANY   => { type => 'LOGICAL',             init => '.FALSE.', op => '.OR.',
               combine => sub { my ($acc, $m) = @_; return "$acc=$acc .OR. $m\n"; } },
    SUM   => { type => 'REAL (KIND=8)',       init => '0',       op => '+',
               combine => sub { my ($acc, $m) = @_; return "$acc=$acc+$m\n"; } },
    COUNT => { type => 'INTEGER (KIND=JPIM)', init => '0',       op => '+',
               combine => sub { my ($acc, $m) = @_; return "IF ($m) THEN\n$acc=$acc+1\nENDIF\n"; } },
  );

my %PREFIX = (ANY => 'LLANY', SUM => 'ZZSUM', COUNT => 'IICOUNT');

my @INTRINSIC_NAMES = sort keys %INTRINSIC;

# The XPath predicate fragment matching any of the supported intrinsic names
# on a named-E's N (e.g. 'string (N)="ANY" or string (N)="SUM" or ...').

sub reductionNamePredicate
{
  return join (' or ', map { qq{string (N)="$_"} } @INTRINSIC_NAMES);
}

# parse (fragment => ...) (fxtran::parser::parseFragment) assumes the parsed
# text is *not* wrapped in a <program-unit>, but whether fxtran wraps it
# depends on the fragment's content (e.g. it does when the first statement is
# a plain assignment, but not when it's a bare DO/ENDDO) -- so that helper
# silently drops everything for some fragments. Do the equivalent ourselves,
# handling both shapes: parse "<text>\nEND\n", then strip the trailing
# end-program-stmt (and any trailing whitespace) from whichever container
# (the program-unit if fxtran added one, else <file> itself) holds our code.

sub myParseFragment
{
  my $text = shift;

  chomp ($text);
  my $program = "$text\nEND\n";

  my $xml = eval { &fxtran::run (qw (-construct-tag -no-include), $program) };
  die ($@) if ($@);

  my $doc = 'XML::LibXML'->load_xml (string => $xml);

  my ($file) = &F ('.//file', $doc);
  my ($pu)   = &F ('./program-unit', $file);

  my $container = $pu || $file;

  my @c = $container->childNodes ();

  while (@c && (($c[-1]->nodeName eq 'end-program-stmt')
             || (($c[-1]->nodeName eq '#text') && ($c[-1]->data =~ m/^\s*$/o))))
    {
      pop (@c);
    }

  return @c;
}

# All ANY/SUM/COUNT (...) calls under a given root. A call-shaped R-LT is
# tagged function-R after -canonic, but parens-R for anything we parse
# ourselves (fresh statements never go through -canonic), so accept either.

sub findReduction
{
  my $root = shift;
  my $pred = &reductionNamePredicate ();
  return &F (".//named-E[($pred) and (R-LT/function-R or R-LT/parens-R)]", $root);
}

# The intrinsic name (ANY/SUM/COUNT) a findReduction () node is a call to.

sub intrinsicOf
{
  my $any = shift;
  my ($name) = &F ('./N/n/text()', $any, 1);
  return $name;
}

# Given the (single) mask/array-argument expression of an ANY/SUM/COUNT
# call, locate the array reference carrying the reduced dimension
# (ARR (lo:hi) [, other fixed subscripts]) and return (lo, hi,
# expr-text-with-that-one-subscript-replaced-by $opts->{jlon}).

sub reduceMask
{
  my ($mask, $opts) = @_;

  my @arr = &F ('.//named-E[R-LT/array-R/section-subscript-LT/section-subscript[lower-bound and upper-bound]]', $mask);

  die ("Fxtran::Reduction: cannot find a reducible array reference (indexed by $opts->{kidia}:$opts->{kfdia}) in: " . $mask->textContent . "\n")
    unless (scalar (@arr) == 1);

  my ($arr) = @arr;

  # The XPath above only requires *some* subscript in the list to be a
  # lower:upper range -- any other subscripts (e.g. a fixed KLEV index
  # alongside a KIDIA:KFDIA one) are left untouched in the generated loop.
  # Reject more than one ranging subscript (ambiguous: which one is being
  # reduced?) or a range that isn't exactly $opts->{kidia}:$opts->{kfdia}.

  my @range = &F ('./R-LT/array-R/section-subscript-LT/section-subscript[lower-bound and upper-bound]', $arr);

  die ("Fxtran::Reduction: more than one ranging subscript in reduced array reference: " . $arr->textContent . "\n")
    unless (scalar (@range) == 1);

  my ($range) = @range;

  my ($lo) = &F ('./lower-bound', $range, 2);
  my ($hi) = &F ('./upper-bound', $range, 2);

  die ("Fxtran::Reduction: reduced array reference is not bounded by $opts->{kidia}:$opts->{kfdia}: " . $arr->textContent . "\n")
    unless (($lo eq $opts->{kidia}) && ($hi eq $opts->{kfdia}));

  my $old = $range->textContent;
  my $new = $opts->{jlon};

  my $masktext = $mask->textContent;

  die ("Fxtran::Reduction: ambiguous reduced reference in mask: " . $mask->textContent . "\n")
    unless (($masktext =~ s/\Q$old\E/$new/) == 1);

  return ($lo, $hi, $masktext);
}

# Tag every do-construct in @$nodes (top-level or nested, e.g. inside a
# buildCascade guard) with reduction="<name>,<op>,<init>" (e.g.
# "LLANY1,.OR.,.FALSE." or "ZZSUM1,+,0"), marking it as a generated
# reduction loop (as opposed to e.g. the DO-WHILE transform's own plain
# "DO") and naming the accumulator, combining operator and identity value it
# reduces with. @$tags gives one such string per reduction do-construct, in
# the same order those do-constructs appear in @$nodes (document order) --
# for buildCascade this is one per branch, since each branch's loop is
# generated, and so appears in the text, strictly after the previous one.

sub markReductionLoops
{
  my ($nodes, $tags) = @_;

  my @dc;
  for my $n (@$nodes)
    {
      next if ($n->nodeName eq '#text');

      push @dc, $n if ($n->nodeName eq 'do-construct');
      push @dc, &F ('.//do-construct', $n);
    }

  die ("Fxtran::Reduction: found " . scalar (@dc) . " reduction loop(s) but "
     . scalar (@$tags) . " tag(s) to mark them with\n")
    unless (scalar (@dc) == scalar (@$tags));

  $dc[$_]->setAttribute ('reduction', $tags->[$_]) for (0 .. $#dc);

  return @$nodes;
}

# Build the "ACC=<init> / DO <jlon>=lo,hi / <combine> / ENDDO" node list
# replacing "ACC = <INTRINSIC> (mask)"; always loops over the whole array
# (no short-circuit).

sub buildLoop
{
  my ($acc, $intrinsic, $mask, $opts) = @_;

  my ($lo, $hi, $masktext) = &reduceMask ($mask, $opts);

  my $init    = $INTRINSIC{$intrinsic}{init};
  my $combine = $INTRINSIC{$intrinsic}{combine}->($acc, $masktext);

  my $code = "$acc=$init\nDO $opts->{jlon}=$lo,$hi\n$combine" . "ENDDO\n";

  my $tag = "$acc,$INTRINSIC{$intrinsic}{op},$init";

  return &markReductionLoops ([&myParseFragment ($code)], [$tag]);
}

# For an if-construct whose IF/ELSEIF branches (any number of them, each
# independently ANY/SUM/COUNT) test a reduction, build the code hoisted
# before it: declare-and-init every temp up front, then a short-circuiting
# cascade where only the first branch's reduction is unconditional -- each
# later one is only computed once every earlier branch's guard has been
# shown false (an "! skip" comment stands in for the now-unneeded branch
# body), mirroring how the ELSEIF chain itself would short-circuit.
#
# $branches is an arrayref of hashes, one per branch, in branch order:
#   any       => the ANY/SUM/COUNT (...) named-E node
#   temp      => the accumulator name generated for this branch
#   intrinsic => ANY/SUM/COUNT
#   guard     => the branch's own original condition text, with its
#                intrinsic call substituted by "temp" -- e.g. bare "LLANY2"
#                for a plain ANY (...) condition, or "ZZSUM1 > 0" for
#                SUM (...) > 0. This is what later branches test to decide
#                whether they still need computing, so it must stay a valid
#                LOGICAL expression regardless of the branch's own
#                (possibly non-LOGICAL) accumulator type.

sub buildCascade
{
  my ($branches, $opts) = @_;

  my $code = join ('', map { "$_->{temp}=" . $INTRINSIC{$_->{intrinsic}}{init} . "\n" } @$branches) . "\n";

  for my $i (0 .. $#$branches)
    {
      my $b = $branches->[$i];

      my ($mask) = &F ('./R-LT/*/element-LT/element', $b->{any});
      my ($lo, $hi, $masktext) = &reduceMask ($mask, $opts);
      my $combine = $INTRINSIC{$b->{intrinsic}}{combine}->($b->{temp}, $masktext);

      if ($i == 0)
        {
          $code .= "DO $opts->{jlon}=$lo,$hi\n$combine" . "ENDDO\n\n";
        }
      else
        {
          $code .= "IF ($branches->[$i - 1]{guard}) THEN\n! skip\nELSE\n\n"
                 . "DO $opts->{jlon}=$lo,$hi\n$combine" . "ENDDO\n\n";
        }
    }

  $code .= "ENDIF\n" x $#$branches;

  my @tag = map { "$_->{temp},$INTRINSIC{$_->{intrinsic}}{op},$INTRINSIC{$_->{intrinsic}}{init}" } @$branches;

  return &markReductionLoops ([&myParseFragment ($code)], \@tag);
}

=head2 reduceAnyIntrinsic

  &Fxtran::Reduction::reduceAnyIntrinsic ($pu, \%opts);

Transforms every C<ANY>/C<SUM>/C<COUNT> reduction found in the (single)
program-unit of document C<$pu> into an explicit loop, in place. C<%opts>
holds the identifier names to use: C<jlon>, C<kidia>, C<kfdia>.

=cut

sub reduceAnyIntrinsic
{
  my ($pu, $opts) = @_;

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  # Fresh temporaries (LLANY1, LLANY2, ..., ZZSUM1, ..., IICOUNT1, ...), one
  # per generated T-decl-stmt, appended after the last existing declaration.
  # Each intrinsic's family is counted (and named) independently.

  my %ntemp;
  my ($lastDecl) = &F ('./T-decl-stmt[last ()]', $dp);

  my $newTemp = sub
    {
      my $intrinsic = shift;

      $ntemp{$intrinsic}++;
      my $name = $PREFIX{$intrinsic} . $ntemp{$intrinsic};

      my $decl = &s ("$INTRINSIC{$intrinsic}{type} :: $name");
      my $nl   = &t ("\n");

      $lastDecl->parentNode->insertAfter ($nl, $lastDecl);
      $lastDecl->parentNode->insertAfter ($decl, $nl);

      $lastDecl = $decl;

      return $name;
    };

  # 1. DO WHILE (cond) -> DO / IF (.NOT. (cond)) THEN / EXIT / ENDIF.
  #    Generic rewrite, applied to every DO WHILE regardless of whether cond
  #    mentions a reduction -- any reduction call inside cond survives
  #    inside the new if-construct's condition and is picked up by the next
  #    pass, which runs after this one.

  for my $dostmt (&F ('.//do-stmt[test-E]', $ep))
    {
      my $doconstruct = $dostmt->parentNode;

      my ($test) = &F ('./test-E', $dostmt);
      my $condtext = $test->textContent;

      my ($newdo) = &F ('./do-stmt', (&myParseFragment ("DO\nENDDO\n"))[0]);
      my $anchor  = $dostmt->nextSibling;

      $doconstruct->replaceChild ($newdo, $dostmt);

      $doconstruct->insertBefore (&t ("\n"), $anchor);

      for my $n (&myParseFragment ("IF (.NOT. ($condtext)) THEN\nEXIT\nENDIF\n"))
        {
          $doconstruct->insertBefore ($n, $anchor);
        }
    }

  # 2. Every if-construct with at least one reduction call in an IF/ELSEIF
  #    branch: hoist the short-circuiting cascade (see buildCascade) before
  #    it, then replace each branch's reduction call with a reference to its
  #    temp.

  my $pred = &reductionNamePredicate ();

  for my $construct (&F (".//if-construct[.//condition-E//named-E[($pred) and (R-LT/function-R or R-LT/parens-R)]]", $ep))
    {
      my @branchstmt = &F ('./if-block/if-then-stmt | ./if-block/else-if-stmt', $construct);

      my @branch;
      for my $stmt (@branchstmt)
        {
          my ($any) = &findReduction ($stmt);
          die ("Fxtran::Reduction: mixed reduction / non-reduction conditions in the same if-construct are not supported\n")
            unless ($any);

          my $intrinsic = &intrinsicOf ($any);
          my $temp = $newTemp->($intrinsic);

          my ($condE) = &F ('./condition-E', $stmt);
          my $condtext = $condE->textContent;
          my $anytext  = $any->textContent;

          (my $guard = $condtext) =~ s/\Q$anytext\E/$temp/;

          push @branch, { any => $any, temp => $temp, guard => $guard, intrinsic => $intrinsic };
        }

      for my $n (&buildCascade (\@branch, $opts))
        {
          $construct->parentNode->insertBefore ($n, $construct);
        }

      $construct->parentNode->insertBefore (&t ("\n"), $construct);

      for my $b (@branch)
        {
          $b->{any}->replaceNode (&e ($b->{temp}));
        }
    }

  # 3. Every remaining reduction call is a plain assignment: reuse the LHS
  #    as the accumulator and replace the whole statement.

  for my $any (&findReduction ($ep))
    {
      my $stmt = &Fxtran::stmt ($any);
      my ($mask) = &F ('./R-LT/*/element-LT/element', $any);

      die ("Fxtran::Reduction: don't know how to reduce " . &intrinsicOf ($any) . " (...) found in a " . $stmt->nodeName . "\n")
        unless ($stmt->nodeName eq 'a-stmt');

      my ($acc) = &F ('./E-1', $stmt, 2);
      my $intrinsic = &intrinsicOf ($any);

      for my $n (&buildLoop ($acc, $intrinsic, $mask, $opts))
        {
          $stmt->parentNode->insertBefore ($n, $stmt);
        }

      $stmt->unbindNode ();
    }
}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
