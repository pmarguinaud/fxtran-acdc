package Fxtran::Reduction;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Reduction

=head1 DESCRIPTION

Rewrites C<ANY (mask)> reductions on a 1-D array section (bounded by the
routine's own C<KIDIA:KFDIA>-like index pair) into an explicit loop, without
altering the calculations:

  LLDO=ANY (IITER (KIDIA:KFDIA)<IITERMAX)

becomes

  LLDO=.FALSE.
  DO JLON=KIDIA,KFDIA
    LLDO=LLDO .OR. IITER (JLON)<IITERMAX
  ENDDO

Three contexts are handled, in this order:

=over 4

=item * C<DO WHILE (cond)> is first rewritten, generically (regardless of
whether C<cond> mentions C<ANY>), into C<DO> / C<IF (.NOT. (cond)) THEN> /
C<EXIT> / C<ENDIF>; any C<ANY (...)> inside C<cond> then falls into the
C<IF>/C<ELSEIF> case below.

=item * A plain assignment C<ACC=ANY (mask)> reuses C<ACC> as the reduction
accumulator and replaces the whole statement with the init+loop.

=item * An C<IF>/C<ELSEIF> chain (any number of branches) whose conditions
test C<ANY (mask)> gets a short-circuiting cascade of reduction loops hoisted
before the C<if-construct>: only the first branch's reduction is
unconditional, each later one is only computed once every earlier branch's
temp has been shown false, mirroring the C<ELSEIF> chain's own
short-circuiting. Each branch's C<ANY (...)> is then replaced by a reference
to its temp.

=back

Every generated reduction C<do-construct> (top-level or nested inside a
cascade guard) is tagged with the attribute C<reduction="1">.

Scope: only a single 1-D array reference, indexed by the exact
C<$opts-E<gt>{kidia}>:C<$opts-E<gt>{kfdia}> bounds, is supported per mask;
anything else (rank > 1, unrelated bounds, no reducible reference, a
non-C<ANY> condition mixed into an otherwise-C<ANY> C<IF>/C<ELSEIF> chain, or
an C<ANY (...)> found outside a supported context) makes the module C<die>
rather than silently produce incorrect Fortran.

=head1 FUNCTIONS

=cut

use strict;

use XML::LibXML;

use fxtran;
use fxtran::xpath;

use Fxtran;

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

# All ANY (mask) calls under a given root. A call-shaped R-LT is tagged
# function-R after -canonic, but parens-R for anything we parse ourselves
# (fresh statements never go through -canonic), so accept either.

sub findAny
{
  my $root = shift;
  return &F ('.//named-E[string (N)="ANY" and (R-LT/function-R or R-LT/parens-R)]', $root);
}

# Given the (single) mask expression of an ANY call, locate the array
# reference carrying the reduced dimension (ARR (lo:hi)) and return
# (lo, hi, mask-text-with-that-reference-indexed-by $opts->{jlon}).

sub reduceMask
{
  my ($mask, $opts) = @_;

  my @arr = &F ('.//named-E[R-LT/array-R/section-subscript-LT/section-subscript[lower-bound and upper-bound]]', $mask);

  die ("Fxtran::Reduction: cannot find a reducible 1-D array reference in mask: " . $mask->textContent . "\n")
    unless (scalar (@arr) == 1);

  my ($arr) = @arr;

  # Basic sanity checks, since the XPath above only requires *some*
  # subscript in the list to be a lower:upper range -- it doesn't rule out
  # e.g. a 2-D reference like PA ($opts->{kidia}:$opts->{kfdia}, JLEV), nor an
  # unrelated range that happens to look like one. Scope is "1-D array
  # indexed by $opts->{kidia}:$opts->{kfdia}", so check both explicitly and
  # fail loudly otherwise.

  my @sub = &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $arr);

  die ("Fxtran::Reduction: reduced array reference is not rank 1: " . $arr->textContent . "\n")
    unless (scalar (@sub) == 1);

  my ($lo)   = &F ('.//lower-bound', $arr, 2);
  my ($hi)   = &F ('.//upper-bound', $arr, 2);
  my ($name) = &F ('./N/n/text()', $arr, 2);

  die ("Fxtran::Reduction: reduced array reference is not bounded by $opts->{kidia}:$opts->{kfdia}: " . $arr->textContent . "\n")
    unless (($lo eq $opts->{kidia}) && ($hi eq $opts->{kfdia}));

  my $old = $arr->textContent;
  my $new = "$name ($opts->{jlon})";

  my $masktext = $mask->textContent;

  die ("Fxtran::Reduction: ambiguous reduced reference in mask: " . $mask->textContent . "\n")
    unless (($masktext =~ s/\Q$old\E/$new/) == 1);

  return ($lo, $hi, $masktext);
}

# Tag every do-construct in @nodes (top-level or nested, e.g. inside a
# buildCascade guard) with reduction="1", marking it as a generated OR-
# reduction loop (as opposed to e.g. the DO-WHILE transform's own plain "DO").

sub markReductionLoops
{
  my @nodes = @_;

  for my $n (@nodes)
    {
      next if ($n->nodeName eq '#text');

      $n->setAttribute ('reduction', '1') if ($n->nodeName eq 'do-construct');

      $_->setAttribute ('reduction', '1') for (&F ('.//do-construct', $n));
    }

  return @nodes;
}

# Build the "ACC=.FALSE. / DO <jlon>=lo,hi / ACC=ACC .OR. mask (<jlon>) / ENDDO"
# node list replacing "ACC = ANY (mask)"; always loops over the whole array
# (no short-circuit).

sub buildLoop
{
  my ($acc, $mask, $opts) = @_;

  my ($lo, $hi, $masktext) = &reduceMask ($mask, $opts);

  my $code = "$acc=.FALSE.\nDO $opts->{jlon}=$lo,$hi\n$acc=$acc .OR. $masktext\nENDDO\n";

  return &markReductionLoops (&myParseFragment ($code));
}

# For an if-construct whose IF/ELSEIF branches (any number of them) each test
# ANY (mask), build the code hoisted before it: declare-and-init every temp
# up front, then a short-circuiting cascade where only the first branch's
# reduction is unconditional -- each later one is only computed once every
# earlier temp has been shown false (an "! skip" comment stands in for the
# now-unneeded branch body), mirroring how the ELSEIF chain itself would
# short-circuit. $any and $temp are parallel arrays, one per branch, in
# branch order.

sub buildCascade
{
  my ($any, $temp, $opts) = @_;

  my $code = join ('', map { "$_=.FALSE.\n" } @$temp) . "\n";

  for my $i (0 .. $#$any)
    {
      my ($mask) = &F ('./R-LT/*/element-LT/element', $any->[$i]);
      my ($lo, $hi, $masktext) = &reduceMask ($mask, $opts);

      if ($i == 0)
        {
          $code .= "DO $opts->{jlon}=$lo,$hi\n$temp->[$i]=$temp->[$i] .OR. $masktext\nENDDO\n\n";
        }
      else
        {
          $code .= "IF ($temp->[$i - 1]) THEN\n! skip\nELSE\n\n"
                 . "DO $opts->{jlon}=$lo,$hi\n$temp->[$i]=$temp->[$i] .OR. $masktext\nENDDO\n\n";
        }
    }

  $code .= "ENDIF\n" x $#$any;

  return &markReductionLoops (&myParseFragment ($code));
}

=head2 reduceAnyIntrinsic

  &Fxtran::Reduction::reduceAnyIntrinsic ($d, \%opts);

Transforms every C<ANY (mask)> reduction found in the (single) program-unit
of document C<$d> into an explicit loop, in place. C<%opts> holds the
identifier names to use: C<jlon>, C<kidia>, C<kfdia>.

=cut

sub reduceAnyIntrinsic
{
  my ($d, $opts) = @_;

  my ($pu) = &F ('.//program-unit', $d);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  # Fresh LOGICAL temporaries (LLANY1, LLANY2, ...), one per generated
  # T-decl-stmt, appended after the last existing declaration.

  my $ntemp = 0;
  my ($lastDecl) = &F ('./T-decl-stmt[last ()]', $dp);

  my $newTemp = sub
    {
      $ntemp++;
      my $name = "LLANY$ntemp";

      my $decl = &s ("LOGICAL :: $name");
      my $nl   = &t ("\n");

      $lastDecl->parentNode->insertAfter ($nl, $lastDecl);
      $lastDecl->parentNode->insertAfter ($decl, $nl);

      $lastDecl = $decl;

      return $name;
    };

  # 1. DO WHILE (cond) -> DO / IF (.NOT. (cond)) THEN / EXIT / ENDIF.
  #    Generic rewrite, applied to every DO WHILE regardless of whether cond
  #    mentions ANY -- any ANY (...) inside cond survives inside the new
  #    if-construct's condition and is picked up by the next pass, which
  #    runs after this one.

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

  # 2. Every if-construct with at least one ANY (...) in an IF/ELSEIF branch:
  #    hoist the short-circuiting cascade (see buildCascade) before it, then
  #    replace each branch's ANY (...) with a reference to its temp.

  for my $construct (&F ('.//if-construct[.//condition-E//named-E[string (N)="ANY" and (R-LT/function-R or R-LT/parens-R)]]', $ep))
    {
      my @branch = &F ('./if-block/if-then-stmt | ./if-block/else-if-stmt', $construct);

      my @any = map
        {
          my ($a) = &findAny ($_);
          die ("Fxtran::Reduction: mixed ANY / non-ANY conditions in the same if-construct are not supported\n")
            unless ($a);
          $a;
        } @branch;

      my @temp = map { $newTemp->() } @branch;

      for my $n (&buildCascade (\@any, \@temp, $opts))
        {
          $construct->parentNode->insertBefore ($n, $construct);
        }

      $construct->parentNode->insertBefore (&t ("\n"), $construct);

      for my $i (0 .. $#any)
        {
          $any[$i]->replaceNode (&e ($temp[$i]));
        }
    }

  # 3. Every remaining ANY (...) is a plain assignment: reuse the LHS as the
  #    accumulator and replace the whole statement.

  for my $any (&findAny ($ep))
    {
      my $stmt = &Fxtran::stmt ($any);
      my ($mask) = &F ('./R-LT/*/element-LT/element', $any);

      die ("Fxtran::Reduction: don't know how to reduce ANY (...) found in a " . $stmt->nodeName . "\n")
        unless ($stmt->nodeName eq 'a-stmt');

      my ($acc) = &F ('./E-1', $stmt, 2);

      for my $n (&buildLoop ($acc, $mask, $opts))
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
