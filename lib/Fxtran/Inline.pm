package Fxtran::Inline;

=head1 NAME

Fxtran::Inline

=head1 DESCRIPTION

The purpose of this module is to provide functions to inline subroutine calls.

=head1 FUNCTIONS

=cut

use FileHandle;
use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Ref;
use Fxtran::Decl;
use Fxtran::DrHook;

# Declarations moved to callers, and the name of the routine they come from (by unique_key)
my %MOVED;

sub dieInline
{

=head2 dieInline

Abort inlining with a message giving the callee, the caller, the CALL statement
and the reason.

=cut

  my ($ctx, $message) = @_;
  (my $call = $ctx->{call}) =~ s/\s+/ /go;
  die ("Inlining $ctx->{callee} into $ctx->{caller} ($call): $message\n");
}

sub strip
{
  my $text = shift;
  $text =~ s/\s+//go;
  return $text;
}

sub bindArguments
{

=head2 bindArguments

Bind the dummy arguments of C<$s2> to the actual arguments of C<$call>, positionally
or by keyword. Return the list of dummy argument names, and a hash mapping each dummy
argument to its actual argument expression (absent arguments are not in the hash).

=cut

  my ($ctx, $s2, $call) = @_;

  my @da = &F ('./dummy-arg-LT/arg-N/N/n/text()', $s2, 1);
  my %da = map { ($_, 1) } @da;

  my %bind;

  my $i = 0;
  for my $arg (&F ('./arg-spec/arg', $call))
    {
      my $da;
      if (($da) = &F ('./arg-N/k/text()', $arg, 1))
        {
          &dieInline ($ctx, "no dummy argument named $da") unless ($da{$da});
        }
      else
        {
          ($da = $da[$i++]) or &dieInline ($ctx, "too many actual arguments");
        }

      my ($aa) = &F ('./ANY-E', $arg);

      &dieInline ($ctx, "dummy $da: unsupported actual argument " . $arg->textContent) unless ($aa);
      &dieInline ($ctx, "dummy $da: bound twice") if ($bind{$da});

      $bind{$da} = $aa;
    }

  return (\@da, \%bind);
}

sub lowerBounds
{

=head2 lowerBounds

Return the lower bounds (as text, without whitespace) of an C<array-spec> element;
the default lower bound is C<1>. C<$map> may map names to the text replacing them,
so that the bounds of a dummy argument can be expressed with actual arguments.

=cut

  my ($as, $map) = @_;

  my @lb;

  for my $ss (&F ('./shape-spec-LT/shape-spec', $as))
    {
      my ($lb) = &F ('./lower-bound/ANY-E', $ss);

      if (! $lb)
        {
          push @lb, '1';
        }
      elsif ($map && ($lb->nodeName eq 'named-E') && (! &F ('./R-LT', $lb)) && $map->{$lb->textContent})
        {
          push @lb, $map->{$lb->textContent};
        }
      else
        {
          push @lb, &strip ($lb->textContent);
        }
    }

  return @lb;
}

sub arraySpec
{

=head2 arraySpec

Return the C<array-spec> of the entity declaration C<$en_decl>, if any.

=cut

  my $en_decl = shift;
  my ($as) = &F ('./array-spec', $en_decl);
  ($as) = &F ('./attribute/array-spec', &Fxtran::stmt ($en_decl)) unless ($as);
  return $as;
}

sub describeDummies
{

=head2 describeDummies

Read the declarations of the dummy arguments of the inlined routine: OPTIONAL attribute,
rank and lower bounds (expressed with the actual arguments when the bound is a dummy
argument). Die for unsupported dummy arguments (undeclared, VALUE).

=cut

  my ($ctx, $dp2, $da, $bind) = @_;

  my %map = map { ($_, &strip ($bind->{$_}->textContent)) }
            grep { $bind->{$_}->nodeName =~ m/^(?:named|literal)-E$/o } keys (%$bind);

  my %info;

  for my $da (@$da)
    {
      my ($en_decl) = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]', $da, $dp2);

      &dieInline ($ctx, "dummy $da: declaration not found") unless ($en_decl);

      my $decl = &Fxtran::stmt ($en_decl);

      my %attr = map { ($_, 1) } &F ('./attribute/attribute-N', $decl, 1);

      &dieInline ($ctx, "dummy $da: VALUE attribute is not supported") if ($attr{VALUE});

      my $as = &arraySpec ($en_decl);
      my @lb = $as ? &lowerBounds ($as, \%map) : ();

      $info{$da} = {decl => $decl, en_decl => $en_decl, optional => $attr{OPTIONAL},
                    rank => scalar (@lb), lb => \@lb};

      &dieInline ($ctx, "dummy $da: no actual argument, and not OPTIONAL")
        unless ($bind->{$da} || $attr{OPTIONAL});
    }

  return \%info;
}

sub checkSpecificationPart
{

=head2 checkSpecificationPart

Die if the specification part of the inlined routine contains statements which cannot
be moved to the caller (only USE statements, type declarations and includes are
supported).

=cut

  my ($ctx, $d2) = @_;

  for my $node (&F ('./specification-part/use-part/node()|./specification-part/declaration-part/node()', $d2))
    {
      my $nn = $node->nodeName;
      next if ($nn =~ m/^(?:#text|C|cpp|use-stmt|T-decl-stmt|include)$/o);
      &dieInline ($ctx, "unsupported statement in callee: " . $node->textContent);
    }
}

sub doLoops
{

=head2 doLoops

Return the XPath expression selecting the DO loops on variable C<$N>, along axis C<$axis>.

=cut

  my ($axis, $N) = @_;
  return join ('|', map { "$axis$_\[./*/do-V/named-E[string(N)=\"$N\"]]" } qw (do-construct do-label-construct));
}

sub isDoVariableOnly
{

=head2 isDoVariableOnly

Tell whether the variable C<$N> is used in C<$ep> only as the variable of DO loops,
referenced only inside these loops, and whether these loops are not nested.

=cut

  my ($N, $ep) = @_;

  for my $expr (&F ('.//named-E[string(N)="?"]', $N, $ep))
    {
      return unless (&F (&doLoops ('ancestor::', $N), $expr));
    }

  for my $loop (&F (&doLoops ('.//', $N), $ep))
    {
      return if (&F (&doLoops ('ancestor::', $N), $loop));
    }

  return 1;
}

sub keepLocalName
{

=head2 keepLocalName

Tell whether the local variable C<$N> of the inlined routine, also declared in the caller,
can keep its name: it has to be used as a DO variable only, in the callee and in the caller,
with the same declaration, and the CALL must not be inside a loop on C<$N>.

=cut

  my ($N, $d1, $d2, $call) = @_;

  my ($dp1) = &F ('./specification-part/declaration-part', $d1);
  my ($ep1) = &F ('./execution-part', $d1);
  my ($dp2) = &F ('./specification-part/declaration-part', $d2);
  my ($ep2) = &F ('./execution-part', $d2);

  my ($decl1) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $N, $dp1);

  return unless ($decl1);
  return unless (&isDoVariableOnly ($N, $ep2));

  my ($decl2) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $N, $dp2);

  return unless (&strip ($decl1->textContent) eq &strip ($decl2->textContent));
  return unless (&isDoVariableOnly ($N, $ep1));
  return if (&F (&doLoops ('ancestor::', $N), $call));

  return 1;
}

sub writtenVariables
{

=head2 writtenVariables

Return the names of the variables which may be modified in C<$ep>: assignment targets,
DO variables, actual arguments of CALL statements (except C<$skip>) and input items. C<$map>
may map names to other names.

=cut

  my ($ep, $map, $skip) = @_;

  my %w;

  for my $N (&F ('.//E-1/named-E/N|.//do-V/named-E/N|.//call-stmt/arg-spec/arg/named-E/N|.//input-item/named-E/N', $ep))
    {
      next if ($skip && (&Fxtran::stmt ($N)->unique_key == $skip->unique_key));
      my $n = $N->textContent;
      $w{$map->{$n} || $n} = 1;
    }

  return \%w;
}

sub keepInvariantName
{

=head2 keepInvariantName

Tell whether the local variable C<$N> of the inlined routine, also declared in the caller,
can keep its name because it always holds the same value: C<$N> is a scalar with the same
declaration in the caller and in the callee; in both, it is modified only by plain assignments
with the same right hand side (the callee's dummy arguments being replaced by their actual
arguments), which references no variable modified in the caller or in the callee (INTENT(IN)
dummy arguments of the caller are never modified); and the callee assigns it.

=cut

  my ($N, $d1, $d2, $bind, $call) = @_;

  my ($dp1) = &F ('./specification-part/declaration-part', $d1);
  my ($ep1) = &F ('./execution-part', $d1);
  my ($dp2) = &F ('./specification-part/declaration-part', $d2);
  my ($ep2) = &F ('./execution-part', $d2);

  my ($decl1) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $N, $dp1);
  my ($decl2) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $N, $dp2);

  return unless ($decl1 && $decl2);
  return unless (&strip ($decl1->textContent) eq &strip ($decl2->textContent));
  return if (&F ('./EN-decl-LT/EN-decl/array-spec|./attribute/array-spec', $decl2));

  # Map dummy arguments of the callee to the names of their actual arguments

  my (%map, %base);
  for my $da (keys (%$bind))
    {
      my $aa = $bind->{$da};
      next unless ($aa->nodeName =~ m/^(?:named|literal)-E$/o);
      $map{$da} = &strip ($aa->textContent);
      ($base{$da}) = &F ('./N', $aa, 1) if ($aa->nodeName eq 'named-E');
    }

  # Right hand sides of the assignments of N

  my %rhs;

  for my $em ([$ep2, \%map], [$ep1, {}])
    {
      my ($ep, $m) = @$em;

      for my $expr (&F ('.//named-E[string(N)="?"]', $N, $ep))
        {
          my $p = $expr->parentNode;
          next unless ($p->nodeName =~ m/^(?:E-1|do-V|arg|input-item)$/o);
          return unless (($p->nodeName eq 'E-1') && (! &F ('./R-LT', $expr)));
          my ($rhs) = &F ('../E-2/ANY-E', $p);
          my $text = $rhs->cloneNode (1);
          for my $n (&F ('./descendant-or-self::named-E/N/n/text()', $text))
            {
              $n->setData ($m->{$n->data}) if ($m->{$n->data});
            }
          $rhs{&strip ($text->textContent)}++;
        }

      return unless (%rhs);  # The callee has to assign N
    }

  return unless (scalar (keys (%rhs)) == 1);

  # Variables referenced by the right hand side must not be modified

  my $w1 = &writtenVariables ($ep1, {}, $call);
  my $w2 = &writtenVariables ($ep2, \%base);

  my ($rhs) = &F ('.//a-stmt[./E-1/named-E[string(N)="?"]]/E-2/ANY-E', $N, $ep2);
  for my $V (&F ('./descendant-or-self::named-E/N', $rhs, 1))
    {
      my $W = $base{$V} || $V;
      # INTENT(IN) dummy arguments of the caller cannot be modified
      next if (&F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]'
                 . '/attribute[string(attribute-N)="INTENT"][string(intent-spec)="IN"]', $W, $dp1));
      return if ($w1->{$W} || $w2->{$W} || $w1->{$V} || $w2->{$V});
    }

  return 1;
}

sub renameLocals
{

=head2 renameLocals

Rename the local variables of the inlined routine whose names are also used in the caller
(declarations, including those coming from other routines inlined before, or USE ... ONLY lists),
appending C<_E<lt>CALLEEE<gt>>; DO variables keep their names when possible (see C<keepLocalName>),
and so do variables always holding the same value (see C<keepInvariantName>).
A declaration identical to the callee's, and coming from a previous inlining, is not a clash:
local variables of routines inlined one after the other can be shared, since they are undefined
on entry. Variables keeping their values between calls (SAVE attribute or initialisation) are
shared only by the inlined copies of the same routine.

=cut

  my ($ctx, $d1, $d2, $da, $bind, $call) = @_;

  my ($dp1) = &F ('./specification-part/declaration-part', $d1);
  my ($up1) = &F ('./specification-part/use-part', $d1);
  my ($dp2) = &F ('./specification-part/declaration-part', $d2);

  my %caller = map { ($_, 1) } &F ('./use-stmt/rename-LT/rename/use-N[1]', $up1, 1);

  for my $decl1 (&F ('./T-decl-stmt', $dp1))
    {
      my ($N) = &F ('./EN-decl-LT/EN-decl/EN-N', $decl1, 1);
      my ($decl2) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $N, $dp2);
      if ($decl2 && (my $from = $MOVED{$decl1->unique_key})
                 && (&strip ($decl1->textContent) eq &strip ($decl2->textContent)))
        {
          my $save = &F ('./attribute[string(attribute-N)="SAVE"]|./EN-decl-LT/EN-decl/init-E', $decl2)
                && ! &F ('./attribute[string(attribute-N)="PARAMETER"]', $decl2);
          next if (($from eq $ctx->{callee}) || (! $save));
        }
      $caller{$N} = 1;
    }

  my %dummy = map { ($_, 1) } @$da;

  my @local = grep { ! $dummy{$_} } &F ('./T-decl-stmt/EN-decl-LT/EN-decl/EN-N', $dp2, 1);

  my %local = map { ($_, 1) } @local;

  my %rename;

  for my $N (@local)
    {
      next unless ($caller{$N});
      next if (&keepLocalName ($N, $d1, $d2, $call));
      next if (&keepInvariantName ($N, $d1, $d2, $bind, $call));
      my $M = "${N}_$ctx->{callee}";
      &dieInline ($ctx, "cannot rename local variable $N: $M is declared in the callee") if ($local{$M});
      $rename{$N} = $M;
    }

  return unless (%rename);

  for my $n (&F ('.//named-E/N/n/text()|.//EN-N/N/n/text()', $d2))
    {
      my $M = $rename{$n->data};
      $n->setData ($M) if ($M);
    }
}

sub renumberLabels
{

=head2 renumberLabels

Renumber the labels of the inlined routine which are already used in the caller.
Return a closure returning new label numbers, counting down from 99999.

=cut

  my ($ep1, $ep2) = @_;

  my %used = map { ($_, 1) } (&F ('.//label', $ep1, 1), &F ('.//label', $ep2, 1));

  my $next = 99999;

  my $newLabel = sub
  {
    $next-- while ($used{$next});
    $used{$next} = 1;
    return $next;
  };

  my %caller = map { ($_, 1) } &F ('.//label', $ep1, 1);

  my %renum;

  for my $label (&F ('.//label/text()', $ep2))
    {
      my $l = $label->data;
      next unless ($caller{$l});
      $renum{$l} ||= $newLabel->();
      $label->setData ($renum{$l});
    }

  return $newLabel;
}

sub removeReturn
{

=head2 removeReturn

Remove RETURN statements from the execution part of the inlined routine: the last
statement is simply removed, others are replaced by a GOTO to a CONTINUE statement
added at the end.

=cut

  my ($ep2, $newLabel) = @_;

  my @ret = &F ('.//return-stmt', $ep2);

  return unless (@ret);

  my ($last) = reverse grep { $_->nodeName !~ m/^(?:#text|C)$/o } $ep2->childNodes ();

  if ($last->unique_key == $ret[-1]->unique_key)
    {
      &removeStmt (pop (@ret));
    }

  return unless (@ret);

  my $label = $newLabel->();

  for my $ret (@ret)
    {
      $ret->replaceNode (&s ("GOTO $label"));
    }

  $ep2->appendChild ($_) for (&t ("\n"), &n ("<label>$label</label>"), &t (" "), &s ("CONTINUE"), &t ("\n"));
}

sub parenthesize
{

=head2 parenthesize

Return a copy of the expression C<$e>, to replace the expression C<$context>; parenthesise
it if it is an operator expression, unless C<$context> is a whole expression by itself (right
hand side of an assignment, argument, subscript or bound, DO loop control, condition).

=cut

  my ($e, $context) = @_;

  return $e->cloneNode (1) unless ($e->nodeName eq 'op-E');

  return $e->cloneNode (1)
    if ($context->parentNode->nodeName =~ m/^(?:E-2|arg|element|lower-bound|upper-bound|stride|step|condition-E|mask-E|case-E)$/o);

  my $p = &n ('<parens-E>(<x/>)</parens-E>');
  my ($x) = &F ('./x', $p);
  $x->replaceNode ($e->cloneNode (1));

  return $p;
}

sub isTriplet
{
  my $ss = shift;
  return (&F ('./text()[contains(.,":")]', $ss) || (&strip ($ss->textContent) eq ':')) ? 1 : 0;
}

sub substituteDesignator
{

=head2 substituteDesignator

Return the expression replacing the occurrence C<$e> of dummy argument C<$da>, when the
actual argument C<$aa> is a variable designator. The actual's references are inserted
before the dummy's; if the dummy is an array, its subscripts take the place of the section
subscripts of the actual (or follow a whole array). Lower bounds of the actual and of the
dummy are assumed to be equal, unless they are explicit and differ.

The result is built from a copy of the occurrence, so that its layout is preserved.

=cut

  my ($ctx, $d1, $e, $da, $aa, $info) = @_;

  return $aa->cloneNode (1) unless (&F ('./R-LT/ANY-R', $e));

  my $new = $e->cloneNode (1);

  my ($N) = &F ('./N', $new);
  my ($Na) = &F ('./N', $aa);
  $N->replaceNode ($Na->cloneNode (1));

  my ($rlt) = &F ('./R-LT', $new);
  my @rd = &F ('./ANY-R', $rlt);
  my @ra = map { $_->cloneNode (1) } &F ('./R-LT/ANY-R', $aa);

  # Actual arguments of calls created from function references are not canonic
  &Fxtran::Ref::parensToArrayRef ($ra[-1]) if (@ra);
  &Fxtran::Ref::parensToArrayRef ($rd[0]);

  if (($rd[0]->nodeName eq 'array-R') && $info->{rank})
    {
      my $head = $rd[0];

      my @ssd = &F ('./section-subscript-LT/section-subscript', $head);

      if (@ra && ($ra[-1]->nodeName eq 'array-R'))
        {
          # Section of an array: the section takes the place of the dummy's subscripts
          my $section = pop (@ra);
          $head->replaceNode ($section);
          $rd[0] = $section;

          my @free = grep { &isTriplet ($_) } &F ('./section-subscript-LT/section-subscript', $section);

          &dieInline ($ctx, "dummy $da: array element passed to an array dummy argument (sequence association)")
            unless (@free);
          &dieInline ($ctx, "dummy $da: rank mismatch between " . $aa->textContent . " and dummy argument")
            unless (scalar (@free) == scalar (@ssd));

          for my $i (0 .. $#free)
            {
              if (&strip ($free[$i]->textContent) eq ':')
                {
                  $free[$i]->replaceNode ($ssd[$i]->cloneNode (1));
                  next;
                }
              &dieInline ($ctx, "dummy $da: stride in " . $aa->textContent . " is not supported")
                if (&F ('./stride', $free[$i]));
              if (my ($lb) = &F ('./lower-bound', $free[$i], 1))
                {
                  $lb = &strip ($lb);
                  &dieInline ($ctx, "dummy $da: lower bound $lb of " . $aa->textContent
                                  . " differs from lower bound $info->{lb}[$i] of dummy argument")
                    unless ($lb eq $info->{lb}[$i]);
                }
              $free[$i]->replaceNode ($ssd[$i]->cloneNode (1));
            }
        }
      else
        {
          # Whole array; check rank and lower bounds when the declaration is available
          if ((! @ra) && (my ($en_decl) = &F ('./specification-part/declaration-part/T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]',
                                               &F ('./N', $aa, 1), $d1)))
            {
              if (my $as = &arraySpec ($en_decl))
                {
                  my @lb = &lowerBounds ($as);
                  &dieInline ($ctx, "dummy $da: rank mismatch between " . $aa->textContent . " and dummy argument")
                    unless (scalar (@lb) == $info->{rank});
                  for my $i (0 .. $#lb)
                    {
                      next unless (($lb[$i] =~ m/^\d+$/o) && ($info->{lb}[$i] =~ m/^\d+$/o));
                      &dieInline ($ctx, "dummy $da: lower bound $lb[$i] of " . $aa->textContent
                                      . " differs from lower bound $info->{lb}[$i] of dummy argument")
                        unless ($lb[$i] == $info->{lb}[$i]);
                    }
                }
            }
        }
    }

  $rlt->insertBefore ($_, $rd[0]) for (@ra);

  return $new;
}

sub substituteOccurrence
{

=head2 substituteOccurrence

Replace the occurrence C<$e> of dummy argument C<$da> by its actual argument C<$aa>.
Variables are composed with the references of the dummy (see C<substituteDesignator>);
constants and expressions are copied (operator expressions are parenthesised when needed,
see C<parenthesize>), and cannot be referenced with subscripts or components.

=cut

  my ($ctx, $d1, $e, $da, $aa, $info) = @_;

  my $new;

  if (($aa->nodeName eq 'named-E') && (! &F ('./R-LT/function-R', $aa)))
    {
      $new = &substituteDesignator ($ctx, $d1, $e, $da, $aa, $info);
    }
  else
    {
      &dieInline ($ctx, "dummy $da: reference " . $e->textContent . " to an expression actual argument")
        if (&F ('./R-LT', $e));
      $new = &parenthesize ($aa, $e);
    }

  $e->replaceNode ($new);
}

sub removeEntity
{

=head2 removeEntity

Remove the declaration of an entity: the whole statement if it declares this entity only.

=cut

  my $en_decl = shift;

  my @en_decl = &F ('preceding-sibling::EN-decl|following-sibling::EN-decl', $en_decl);

  if (! @en_decl)
    {
      &removeStmt (&Fxtran::stmt ($en_decl));
      return;
    }

  my $sep = $en_decl->previousSibling || $en_decl->nextSibling;
  $sep->unbindNode () if ($sep && ($sep->nodeName eq '#text'));
  $en_decl->unbindNode ();
}

sub removeStmt
{

=head2 removeStmt

Remove a statement node and all surrounding whitespace/text nodes up to and including the end-of-line, trimming the preceding newline to keep formatting clean.

=cut

  my $stmt = shift;
  # Remove everything until eol
 
  my @n;
  for (my $n = $stmt; $n; $n = $n->nextSibling)
    {
      push @n, $n;
      if ($n->nodeName eq '#text')
        {
          last if ($n->data =~ m/\n/o);
        }
    }
  
  # Remove everything from start of line

  for (my $n = $stmt->previousSibling; $n; $n = $n->previousSibling)
    {
      if ($n->nodeName eq '#text')
        {
          if ($n->data =~ m/\n/o)
            {
              my $t = $n->data;
              $t =~ s/\n\s*$/\n/o;
              $n->setData ($t);
              last;
            }
        }
      else
        {
          last;
        }
      push @n, $n;
    }
  
  for (@n)
    {
      $_->unbindNode ();
    }
}

sub inlineSingleCall
{

=head2 inlineSingleCall

Inline a single CALL statement: C<$d1> is the calling program unit, C<$d2> a copy of the
program unit being called (it is modified), C<$s2> its subroutine statement, C<$n2> its
name, and C<$call> the CALL statement to be replaced by the executable statements of C<$d2>.

The steps are:

=over 4

=item

Bind dummy arguments to actual arguments (by position or keyword).

=item

With option C<renameLocals>, rename local variables of the callee (see C<renameLocals>).
Renumber labels of the callee already used in the caller. Replace RETURN statements by a
GOTO to the end of the inlined code.

=item

Replace C<PRESENT (X)> by C<.TRUE.> or C<.FALSE.>, then all occurrences of dummy arguments by
their actual arguments. All occurrences are collected before any substitution, so that names
coming from actual arguments are never substituted again.

=item

Simplify the code (constant conditions, C<MERGE> with a constant mask); statements still
referencing absent optional arguments are replaced by C<STOP 1>.

=item

Remove dummy argument declarations; move USE statements, declarations and includes to
the caller (option C<inlineDeclarations>); insert the executable statements in place of the
CALL (option C<comment> adds comments around the inlined code).

=back

Actual arguments are substituted textually. In particular:

=over 4

=item

Lower bounds of actual arrays and of dummy arrays are assumed to be equal; inlining fails
when they are explicit and different (C<CALL S (X (:, N1:N2))>, or whole arrays declared with
different integer lower bounds).

=item

Shapes of derived type components and of variables not declared in the caller (module or
host variables) are not known: their rank and lower bounds are assumed to match the dummy's.

=item

An expression passed as actual argument is evaluated where the dummy is used, that is, after
the inlined code may have modified the variables it references.

=item

Inlining fails for strided sections, array elements passed to array dummy arguments
(sequence association), rank changes and VALUE dummy arguments.

=back

=cut

  my ($d1, $d2, $s2, $n2, $call, %opts) = @_;

# d1 is the outer program unit
# d2 is the program unit being called
# s2 is the subroutine statement of d2
# call is the call statement to be inlined

  my ($dp1) = &F ('./specification-part/declaration-part', $d1);
  my ($ep1) = &F ('./execution-part', $d1);

  my ($dp2) = &F ('./specification-part/declaration-part', $d2);
  my ($up2) = &F ('./specification-part/use-part', $d2);
  my ($ep2) = &F ('./execution-part', $d2);

  my ($caller) = &F ('./subroutine-stmt/subroutine-N|./function-stmt/function-N', $d1, 1);

  my $ctx = {callee => "$n2", caller => $caller, call => $call->textContent};

  my ($da, $bind) = &bindArguments ($ctx, $s2, $call);

  my $info = &describeDummies ($ctx, $dp2, $da, $bind);

  &checkSpecificationPart ($ctx, $d2);

  &renameLocals ($ctx, $d1, $d2, $da, $bind, $call) if ($opts{renameLocals});

  my $newLabel = &renumberLabels ($ep1, $ep2);

  &removeReturn ($ep2, $newLabel);

  # Handle PRESENT intrinsic

  for my $da (@$da)
    {
      for my $present (&F ('.//named-E[translate(string(.)," ","")="PRESENT(?)"]', $da, $ep2))
        {
          $present->replaceNode ($bind->{$da} ? &TRUE () : &FALSE ());
        }
    }

  # Remove dummy arguments declarations

  &removeEntity ($info->{$_}{en_decl}) for (@$da);

  # Replace dummy arguments by actual arguments; collect all occurrences first, and substitute
  # inner occurrences (subscripts) first

  if (my @bound = grep { $bind->{$_} } @$da)
    {
      my @e = &F ('.//named-E[' . join (' or ', map { "string(N)=\"$_\"" } @bound) . ']', $d2);

      for my $e (reverse (@e))
        {
          my ($N) = &F ('./N', $e, 1);
          &substituteOccurrence ($ctx, $d1, $e, $N, $bind->{$N}, $info->{$N});
        }
    }

  # Simplify subroutine (with PRESENT (...) expressions replaced by their values)

  &Fxtran::Construct::apply ($d2);

  # See whether some optional missing arguments remain

  for my $da (grep { ! $bind->{$_} } @$da)
    {
      my @stmt = &F ('.//ANY-stmt[.//named-E[./N/n/text()="?"]]', $da, $ep2);
      for my $stmt (@stmt)
        {
          if ($stmt->nodeName eq 'if-then-stmt')
            {
              my $construct = $stmt->parentNode->parentNode;
              $construct->replaceNode (&s ('STOP 1'));
            }
          elsif ($stmt->nodeName eq 'else-if-stmt')
            {
              &dieInline ($ctx, "dummy $da: absent, and referenced in " . $stmt->textContent);
            }
          else
            {
              $stmt->replaceNode (&s ('STOP 1'));
            }
        }
    }

  my @use = &F ('./use-stmt', $up2);
  my @decl = &F ('./T-decl-stmt', $dp2);
  my @include = &F ('./include', $dp2);

  if (@use || @decl || @include)
    {
      &dieInline ($ctx, "callee has declarations, and option inlineDeclarations is not set")
        unless ($opts{inlineDeclarations});

      for my $use (@use)
        {
          &Fxtran::Decl::use ($d1, $use->cloneNode (1));
          &removeStmt ($use);
        }
      for my $decl (@decl)
        {
          my $clone = $decl->cloneNode (1);
          &Fxtran::Decl::declare ($d1, $clone);
          $MOVED{$clone->unique_key} = "$n2" if ($clone->parentNode);
          &removeStmt ($decl);
        }
      for my $include (@include)
        {
          &Fxtran::Decl::include ($d1, $include->cloneNode (1));
          &removeStmt ($include);
        }
    }

  my @node = &F ('./node()', $ep2);

  # A label on the CALL statement is kept on a CONTINUE statement

  my $prev = $call->previousSibling;
  $prev = $prev->previousSibling if ($prev && ($prev->nodeName eq '#text') && ($prev->data !~ m/\n/o));
  if ($prev && ($prev->nodeName eq 'label'))
    {
      $call->parentNode->insertBefore ($_, $call) for (&s ('CONTINUE'), &t ("\n"));
    }

  # Insert executable statements from inlined routine + a few comments

  $call->parentNode->insertAfter (&t ("\n"), $call);
  if ($opts{comment})
    {
      $call->parentNode->insertAfter (&n ("<C>!----- END INLINE $n2</C>"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);
    }

  for my $node (reverse @node)
    {
      $call->parentNode->insertAfter ($node, $call);
    }

  if ($opts{comment})
    {
      # Comment old code (CALL)
      my @c = split (m/\n/o, $call->textContent ());
      for my $i (reverse (0 .. $#c))
        {
          my $c = $c[$i];
          $c = "! $c";
          $c = &t ($c);
          $c = $c->toString ();
          $call->parentNode->insertAfter (&t ("\n"), $call);
          $call->parentNode->insertAfter (&n ("<C>" . $c . "</C>"), $call);
        }

      $call->parentNode->insertAfter (&t ("\n"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);

      $call->parentNode->insertAfter (&n ("<C>!----- BEGIN INLINE $n2</C>"), $call);
      $call->parentNode->insertAfter (&t ("\n"), $call);
    }


  # Remove CALL statement

  $call->unbindNode ();
}

sub loopElementalSingleCall
{

=head2 loopElementalSingleCall

Wrap a single call to an ELEMENTAL subroutine with DO loops over the array dimensions of its
array arguments, replacing whole-array section subscripts with loop indices. Arrays referenced
in argument expressions are indexed as well. Loop indices and default bounds are given by the
style (C<dim2ind>, C<dim2bnd>); other bounds come from the sections or the declarations.
Missing loop indices are declared (C<integerType> of the style).

Die when the loop indices or bounds of the arrays differ, for sections of arrays whose
declaration is not visible, of components and of assumed-shape arrays, for strides, and when
a loop index is already used by an enclosing loop.

=cut

  my ($d1, $call, %opts) = @_;

  my $style = $opts{style};

  my ($n2) = &F ('./procedure-designator', $call, 1);
  my ($caller) = &F ('./subroutine-stmt/subroutine-N|./function-stmt/function-N', $d1, 1);
  my $ctx = {callee => $n2, caller => $caller, call => $call->textContent};

  my ($FIRST, @IND, @LBND, @UBND);

  for my $expr (&F ('./arg-spec/arg//named-E', $call))
    {
      # Skip expressions removed with a subscript replaced by a loop index
      next unless (&F ('ancestor::call-stmt', $expr));

      my ($N) = &F ('./N', $expr, 1);

      my $section = grep { &isTriplet ($_) } &F ('./R-LT/array-R/section-subscript-LT/section-subscript', $expr);

      if (&F ('./R-LT/component-R', $expr))
        {
          &dieInline ($ctx, "section of a component: " . $expr->textContent) if ($section);
          next;
        }

      my $en_decl = &findDeclaration ($N, $call);

      unless ($en_decl)
        {
          &dieInline ($ctx, "section of $N, whose declaration is not visible") if ($section);
          next;
        }

      next unless (my $as = &arraySpec ($en_decl));

      my ($r) = &F ('./R-LT/ANY-R', $expr);
      &Fxtran::Ref::parensToArrayRef ($r) if ($r);

      my ($ar) = &F ('./R-LT/array-R', $expr);

      my @ssp = &F ('./shape-spec-LT/shape-spec', $as);

      if (! $r)
        {
          $expr->appendChild (my $rlt = &n ('<R-LT/>'));

          $ar = &n ('<array-R>(<section-subscript-LT>'
                   . join (',', ('<section-subscript>:</section-subscript>') x scalar (@ssp))
                   . '</section-subscript-LT>)</array-R>');
          $rlt->appendChild ($ar);
        }
      elsif ((! $ar) || (! $section))
        {
          next;
        }

      my @ssu = &F ('./section-subscript-LT/section-subscript', $ar);

      &dieInline ($ctx, "$N: rank of the reference differs from the rank of the declaration")
        unless (scalar (@ssu) == scalar (@ssp));

      my (@ind, @lbnd, @ubnd);

      for my $i (0 .. $#ssu)
        {
          my $ssu = $ssu[$i];

          next unless (&isTriplet ($ssu));

          &dieInline ($ctx, "$N: stride in section " . $expr->textContent) if (&F ('./stride', $ssu));

          my ($dlb) = &F ('./lower-bound', $ssp[$i], 1);
          my ($dub) = &F ('./upper-bound', $ssp[$i], 1);

          &dieInline ($ctx, "$N: dimension " . ($i + 1) . " is assumed or deferred (no upper bound)") unless (defined ($dub));

          my $ind = $style->dim2ind (&strip ($dub));

          &dieInline ($ctx, "$N: no loop index for dimension $dub") unless ($ind);

          my ($lb) = &F ('./lower-bound', $ssu, 1);
          my ($ub) = &F ('./upper-bound', $ssu, 1);

          if ((! defined ($lb)) && (! defined ($ub)))
            {
              ($lb, $ub) = $style->dim2bnd (&strip ($dub));
              ($lb, $ub) = (defined ($dlb) ? $dlb : '1', $dub) unless (defined ($lb));
            }
          else
            {
              $lb = defined ($dlb) ? $dlb : '1' unless (defined ($lb));
              $ub = $dub unless (defined ($ub));
            }

          $ssu->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>$ind</n></N></named-E></lower-bound></section-subscript>"));

          push @ind, $ind;
          push @lbnd, $lb;
          push @ubnd, $ub;
        }

      # Same indices and bounds for all arrays

      if (defined ($FIRST))
        {
          &dieInline ($ctx, "$N: loop indices (@ind) differ from those of $FIRST (@IND)")
            unless ("@ind" eq "@IND");
          for my $i (0 .. $#IND)
            {
              &dieInline ($ctx, "$N: bounds $lbnd[$i]:$ubnd[$i] differ from those of $FIRST ($LBND[$i]:$UBND[$i])")
                unless ((&strip ($lbnd[$i]) eq &strip ($LBND[$i])) && (&strip ($ubnd[$i]) eq &strip ($UBND[$i])));
            }
        }
      else
        {
          $FIRST = $N;
          @IND = @ind;
          @LBND = @lbnd;
          @UBND = @ubnd;
        }
    }

  return unless (@IND);

  my ($pu) = &F ('ancestor::program-unit[1]', $call);

  for my $ind (@IND)
    {
      &dieInline ($ctx, "loop index $ind is already used by an enclosing loop")
        if (&F (&doLoops ('ancestor::', $ind), $call));
      &Fxtran::Decl::declare ($pu, &s ($style->integerType () . " :: $ind"))
        unless (&findDeclaration ($ind, $call));
    }

  my $do_construct = join ("\n",
                           map ({ "DO $IND[$_] = $LBND[$_], $UBND[$_]" } reverse (0 .. $#IND)),
                           "!",
                           map ({ "ENDDO" } reverse (0 .. $#IND)));

  ($do_construct) = &Fxtran::parse (fragment => $do_construct, fopts => [qw (-line-length 800 -construct-tag)]);

  my ($C) = &F ('.//C', $do_construct);

  $call->replaceNode ($do_construct);

  $C->replaceNode ($call);
}

sub loopElemental
{

=head2 loopElemental

Add DO loops around all calls to an ELEMENTAL subroutine named C<$n2> within program unit C<$d1>.

=cut

  my ($d1, $n2, %opts) = @_;

# Add appropriate loops around calls around calls of ELEMENTAL subroutines

  my @call = &F ('.//call-stmt[string(procedure-designator)="?"]', $n2, $d1);
  for my $call (@call)
    {
      &loopElementalSingleCall ($d1, $call, %opts);
    }
}

sub inlineContainedSubroutine
{

=head2 inlineContainedSubroutine

Inline all calls to the contained subroutine named C<$n2> within program unit C<$d1>, adding DO loops first if the subroutine is ELEMENTAL.

=cut

  my ($d1, $n2, %opts) = @_;

# d1 is the outer program unit
# n2 is the name of the contained routine to be inlined

  # Fxtran::Subroutine to be inlined
  my ($D2) = &F ('.//program-unit[./subroutine-stmt[./subroutine-N/N/n/text()="?"]]', $n2, $d1);
  my ($S2) = &F ('.//subroutine-stmt', $D2);

  if (&F ('./prefix[string(.)="ELEMENTAL"]', $S2))
    {
      &loopElemental ($d1, $n2, %opts);
    }

  # Fxtran::Subroutine calls to be replaced by subroutine contents
  my @call = &F ('.//call-stmt[./procedure-designator/named-E/N/n/text()="?"]', $n2, $d1);

  for my $call (@call)
    {
      &inlineSingleCall ($d1, $D2->cloneNode (1), $S2->cloneNode (1), $n2, $call, %opts, renameLocals => 1);
    }
}

sub sortContainedSubroutines
{

=head2 sortContainedSubroutines

Sort a list of contained subroutine name nodes so that subroutines called by no other contained subroutine are inlined first, using a dependency-counting approach.

=cut

# CONTAINed subroutines to be inlined have to be sorted
# we inline first routines which are called by no other
# subroutine

  my @n2 = @_;  # Text node of subroutine names that we want to inline

  my %ref; # Count how many times a contained routine
           # calls other contained subroutines

  for my $n2 (@n2)
    {
      my @pu = &F ('ancestor::program-unit', $n2);
      my $pu2 = pop (@pu); # pu2 is the program unit of n2

      my ($ep2) = &F ('./execution-part', $pu2);

      for my $n (@n2)
        {
          $ref{$n2->textContent}{$n->textContent} = 
          &F ('.//call-stmt[string(procedure-designator)="?"]', $n->textContent, $ep2) && 1;
        }
    }

  my @s2;

  while (%ref)
    {

      for my $n2 (@n2)
        {
          next unless (my $ref = $ref{$n2->textContent});
 
          my $ok = 1;
          for (values (%$ref))
            {
              $ok &&= ($_ == 0);
            }
 
          if ($ok)
            {
              unshift @s2, $n2;
              delete $ref{$n2->textContent};
            }

        }

      for my $s2 (@s2)
        {
          for my $ref (values (%ref))
            {
              $ref->{$s2->textContent} = 0;
            }
        }

    }

  return @s2;
}

sub findDeclaration
{

=head2 findDeclaration

Return the entity declaration of C<$N> visible from C<$node>: the declaration part of the
innermost program unit containing C<$node> which declares it, if any.

=cut

  my ($N, $node) = @_;

  for my $pu (reverse (&F ('ancestor-or-self::program-unit', $node)))
    {
      my ($en_decl) = &F ('./specification-part/declaration-part/T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]', $N, $pu);
      return $en_decl if ($en_decl);
    }

  return;
}

sub functionResult
{

=head2 functionResult

Return the name of the result variable of the function C<$pu2> named C<$n2>. Without a RESULT
clause, the result is renamed C<E<lt>FE<gt>_RESULT> in the function (C<E<lt>FE<gt>_RESULT2>... if this
name is used), and declared with the type given in the function statement if needed.

=cut

  my ($ctx, $pu2, $n2) = @_;

  my $f = $pu2->firstChild;

  if (my ($r) = &F ('./result-spec/N', $f, 1))
    {
      return $r;
    }

  my %used = map { ($_, 1) } &F ('.//N/n', $pu2, 1);

  my $r = "${n2}_RESULT";
  for (my $i = 2; $used{$r}; $i++)
    {
      $r = "${n2}_RESULT$i";
    }

  my ($ts) = &F ('./intrinsic-T-spec|./derived-T-spec', $f);
  my ($en) =&F ('./specification-part/declaration-part/T-decl-stmt/EN-decl-LT/EN-decl/EN-N/N/n/text()[string(.)="?"]', $n2, $pu2);

  if ($en)
    {
      $en->setData ($r);
    }
  elsif ($ts)
    {
      &Fxtran::Decl::declare ($pu2, &s ($ts->textContent . " :: $r"));
    }
  else
    {
      &dieInline ($ctx, "type of the function result not found");
    }

  $_->setData ($r) for (&F ('.//named-E/N/n/text()[string(.)="?"]', $n2, $pu2));

  return $r;
}

sub functionReferences
{

=head2 functionReferences

Return the references to function C<$n2> in C<$d1>, grouped by statement (a list of
[statement, references] pairs), with the references of each statement in evaluation order:
inner references (in arguments) first, then from left to right.

=cut

  my ($d1, $n2) = @_;

  my (@stmt, %ref);

  for my $expr (&F ('.//named-E[string(N)="?"][./R-LT/function-R]', $n2, $d1))
    {
      my $stmt = &Fxtran::stmt ($expr);
      push @stmt, $stmt unless ($ref{$stmt->unique_key});
      push @{ $ref{$stmt->unique_key} }, $expr;
    }

  my @list;

  for my $stmt (@stmt)
    {
      my @expr = @{ $ref{$stmt->unique_key} };
      my %rank = map { ($expr[$_]->unique_key, $_) } (0 .. $#expr);
      my %inner = map { my $e = $_; ($e->unique_key, { map { ($_->unique_key, 1) } &F ('.//named-E', $e) }) } @expr;

      # Post-order: an expression comes after the references it contains
      @expr = sort { $inner{$a->unique_key}{$b->unique_key} ? +1
                   : $inner{$b->unique_key}{$a->unique_key} ? -1
                   : $rank{$a->unique_key} <=> $rank{$b->unique_key} } @expr;

      push @list, [$stmt, \@expr];
    }

  return @list;
}

sub containedFunctionToSubroutine
{

=head2 containedFunctionToSubroutine

Transform the contained function whose name is the text node C<$f2> into a subroutine with
an extra INTENT(OUT) argument for its result, and replace each reference to it in C<$d1> by
a temporary variable, set by a CALL inserted before the statement. A statement gets one
temporary per reference (C<E<lt>FE<gt>TMP>, C<E<lt>FE<gt>TMP2>... skipping names used in
C<$d1>), the CALL statements being in evaluation order.

Die for references in an ELSE IF or DO WHILE condition, or outside the execution part, for
references of an ELEMENTAL function with an array argument, and for array results whose shape
depends on the dummy arguments.

=cut

  my ($d1, $f2, %opts) = @_;

  my $n2 = $f2->textContent;

  my ($caller) = &F ('./subroutine-stmt/subroutine-N|./function-stmt/function-N', $d1, 1);

  my @pu = &F ('ancestor::program-unit', $f2);
  my $pu2 = pop (@pu);

  my $f = $pu2->firstChild;
  my ($l) = &F ('./end-function-stmt', $pu2);

  my $ctx = {callee => $n2, caller => $caller, call => $f->textContent};

  my $r = &functionResult ($ctx, $pu2, $n2);

  my ($dp2) = &F ('./specification-part/declaration-part', $pu2);

  my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $r, $dp2);

  &dieInline ($ctx, "result $r: declaration not found") unless ($decl);
  &dieInline ($ctx, "result $r: declared with other entities") if (&F ('./EN-decl-LT/EN-decl', $decl) > 1);

  my %dummy = map { ($_, 1) } &F ('./dummy-arg-LT/arg-N', $f, 1);

  if (my $as = &arraySpec (&F ('./EN-decl-LT/EN-decl', $decl)))
    {
      &dieInline ($ctx, "result $r: shape depends on dummy arguments")
        if (grep { $dummy{$_} } &F ('.//named-E/N', $as, 1));
    }

  my $elemental = &F ('./prefix[string(.)="ELEMENTAL"]', $f);

  # Declaration of the temporaries, without INTENT

  my $tdecl = $decl->cloneNode (1);

  # Names already used in the caller

  my %used = map { ($_, 1) } &F ('.//N/n|.//use-N', $d1, 1);

  my @tmp;

  my $tmp = sub
  {
    my $i = shift;
    until ($tmp[$i])
      {
        my $T;
        for (my $j = 1; ; $j++)
          {
            $T = $n2 . 'TMP' . ($j > 1 ? $j : '');
            last unless ($used{$T});
          }
        $used{$T} = 1;
        my $d = $tdecl->cloneNode (1);
        my ($N) = &F ('./EN-decl-LT/EN-decl/EN-N/N/n/text()', $d);
        $N->setData ($T);
        &Fxtran::Decl::declare ($d1, $d);
        push @tmp, $T;
      }
    return $tmp[$i];
  };

  # Replace function references by temporaries set by CALL statements

  for my $item (&functionReferences ($d1, $n2))
    {
      my ($stmt, $expr) = @$item;

      $ctx->{call} = $stmt->textContent;

      &dieInline ($ctx, "recursive function")
        if (grep { $_->isSameNode ($pu2) } &F ('ancestor::program-unit', $stmt));
      &dieInline ($ctx, "referenced in an ELSE IF condition")
        if ($stmt->nodeName eq 'else-if-stmt');
      &dieInline ($ctx, "referenced in a DO WHILE condition")
        if (($stmt->nodeName eq 'do-stmt') && &F ('./test-E', $stmt));
      &dieInline ($ctx, "referenced outside the execution part")
        unless (&Fxtran::stmt_is_executable ($stmt) && &F ('ancestor::execution-part', $stmt));

      for my $i (0 .. $#{$expr})
        {
          my $e = $expr->[$i];

          my @arg = &F ('./R-LT/function-R/element-LT/element|./R-LT/function-R/arg-spec/arg', $e);

          if ($elemental)
            {
              for my $x (map { &F ('.//named-E', $_) } @arg)
                {
                  my ($N) = &F ('./N', $x, 1);
                  my $en_decl = &findDeclaration ($N, $stmt);
                  next unless ($en_decl && &arraySpec ($en_decl));
                  my ($ar) = &F ('./R-LT/array-R', $x);
                  &dieInline ($ctx, "ELEMENTAL function with an array argument ($N)")
                    if ((! $ar) || grep { &isTriplet ($_) } &F ('./section-subscript-LT/section-subscript', $ar));
                }
            }

          my $T = $tmp->($i);

          my $call = &s ("CALL $n2 (X)");
          my ($argspec) = &F ('./arg-spec', $call);
          $_->unbindNode () for ($argspec->childNodes ());

          my $keyword = 0;
          for my $arg (@arg)
            {
              my $a = &n ('<arg/>');
              $a->appendChild ($_->cloneNode (1)) for ($arg->childNodes ());
              $keyword ||= &F ('./arg-N', $a);
              $argspec->appendChild ($_) for (($argspec->hasChildNodes () ? (&t (", ")) : ()), $a);
            }

          my $a = &n ($keyword ? "<arg><arg-N n=\"$r\"><k>$r</k></arg-N>=</arg>" : '<arg/>');
          $a->appendChild (&e ($T));
          $argspec->appendChild ($_) for (($argspec->hasChildNodes () ? (&t (", ")) : ()), $a);

          $stmt->parentNode->insertBefore ($_, $stmt) for ($call, &t ("\n"));
          $e->replaceNode (&e ($T));
        }
    }

  # Add an INTENT to the result

  my ($ts) = &F ('./_T-spec_', $decl);

  $decl->insertAfter ($_, $ts) for (&n ("<attribute><attribute-N>INTENT</attribute-N> (<intent-spec>OUT</intent-spec>)</attribute>"), &t (", "));

  # Add the result as an extra argument

  my ($dal2) = &F ('./dummy-arg-LT', $f);
  $dal2->appendChild ($_) for (($dal2->hasChildNodes () ? (&t (", ")) : ()), &n ("<arg-N><N><n>$r</n></N></arg-N>"));

  # Replace function-stmt / end-function-stmt by subroutine-stmt / end-subroutine-stmt

  my $sub = &n ("<subroutine-stmt>SUBROUTINE <subroutine-N><N><n>" . $n2 . "</n></N></subroutine-N> (<dummy-arg-LT/>)</subroutine-stmt>");

  for (&F ('./dummy-arg-LT', $sub))
    {
      $_->replaceNode ($dal2);
    }

  $f->replaceNode ($sub);

  $l->replaceNode (&n ("<end-subroutine-stmt>ENDSUBROUTINE</end-subroutine-stmt>"));
}

sub inlineContainedSubroutines
{

=head2 inlineContainedSubroutines

Inline all routines from the CONTAINS section. Sort these
routines: inline first routines which are not called by
any other CONTAINed subroutine.

Remove the CONTAINS statement and the CONTAINed subroutines.

=cut

  my ($d1, %opts) = @_;

  my @f2 = &F ('./program-unit/function-stmt/function-N/N/n/text()', $d1);

  for my $f2 (@f2)
    {
      &containedFunctionToSubroutine ($d1, $f2, %opts);
    }

  my @n2 = &F ('./program-unit/subroutine-stmt/subroutine-N/N/n/text()', $d1);

  @n2 = &sortContainedSubroutines (@n2);

  for my $n2 (@n2)
    {
      &inlineContainedSubroutine ($d1, $n2, %opts);

      my @pu = &F ('ancestor::program-unit', $n2);
      my $pu2 = pop (@pu);
      $pu2->unbindNode ();

    }
  
  if ($d1->nodeName eq 'program-unit')
    {
      for (&F ('./contains-stmt', $d1))
        {
          $_->unbindNode ();
        }
    }
  else
    {
      for (&F ('.//program-unit//contains-stmt', $d1))
        {
          $_->unbindNode ();
        }
    }

}

sub suffixVariables
{

=head2 suffixVariables

Append a suffix to the names of all local variables (excluding dummy arguments and style-defined loop indices) in a program unit, updating both declarations and all references.

=cut

  my ($pu, %opts) = @_;

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my $suffix = $opts{suffix};

  # Do not suffix arguments, as they will be replaced anyway

  my @args = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my @en_decl = &F ('./T-decl-stmt/EN-decl-LT/EN-decl/EN-N/N/n/text()', $dp);

  my @skip = ($opts{style}->jlon (), $opts{style}->jlev ());
  my %skip = map { ($_, 1) } (@skip, @args);

  my %N;
  for my $en_decl (@en_decl)
    {
      my $N = $en_decl->data;
      next if ($skip{$N});
      $N{$N} = $N . $suffix;
      $en_decl->setData ($N . $suffix);
    }

  for my $expr 
    (&F ('./subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $pu), 
     &F ('.//named-E/N/n/text()', $pu))
    {
      my $N = $expr->data;
      next unless ($N{$N});
      $expr->setData ($N{$N});
    }

  return \%N;
}

sub inlineExternalSubroutine
{

=head2 inlineExternalSubroutine

Inline external subroutine. Suffix inlined routine variables with inlined routine name.

=cut

  my ($d1, $d2, %opts) = @_;

  my ($dp1) = &F ('./specification-part/declaration-part', $d1);
  my ($ep1) = &F ('./execution-part', $d1);

  my ($D2) = &F ('./object/file/program-unit', $d2);
  my ($S2) = &F ('./subroutine-stmt', $D2);
  my ($n2) = &F ('./subroutine-N/N/n/text()', $S2);
  
  my ($dp2) = &F ('./specification-part/declaration-part', $D2);
  my ($ep2) = &F ('./execution-part', $D2);

  # Subroutine calls to be replaced by subroutine contents
  my @call = &F ('.//call-stmt[./procedure-designator/named-E/N/n/text()="?"]', $n2, $ep1);

  # Record renamed local variables from inlined subroutine
  my @rename;

  for my $i (0 .. $#call)
    {
      my $call = $call[$i];
      my $suffix = "_$n2";
      $suffix .= sprintf ('%3.3d', $i) if (scalar (@call) > 1);
      my $DD2 = $D2->cloneNode (1);
      &Fxtran::DrHook::remove ($DD2);
      my ($SS2) = &F ('./subroutine-stmt', $DD2);
  
      my $rename = &suffixVariables ($DD2, %opts, suffix => $suffix);
      push @rename, $rename;

      for my $N (sort keys (%$rename))
        {
          my @k = &F ('./arg-spec/arg/arg-N/k/text()[string(.)="?"]', $N, $call);
          for my $k (@k)
            {
              $k->setData ($rename->{$N});
            }
        }

      &inlineSingleCall 
        (
          $d1, $DD2, $SS2, $n2, $call, %opts, 
          inlineDeclarations => 1,
        );
    }

  my %N;

  for my $rename (@rename)
    {
      for my $N (sort keys (%$rename)) 
        {
          push @{ $N{$N} }, $rename->{$N};
        }
    }

  # Merge consistent declarations (and remove variable suffix)

  my @ENN = &F ('./T-decl-stmt//EN-N', $dp1, 1);

  my %ENN = map { ($_, 1) } @ENN;

  for my $N (sort keys (%N))
    {
      next if ($ENN{$N}); # Variable already exists in caller

      my @N = @{ $N{$N} };

      my @en_decl = map { &F ('./T-decl-stmt//EN-decl[string(EN-N)="?"]', $_, $dp1) } @N;

      next unless (@en_decl); # May be an argument

      my @as = map { &F ('./array-spec', $_, 1) || '' } @en_decl;
      my $eq = ! grep { $_ ne $as[0] } @as;
      if ($eq)
        {
          # Merge all declarations into a single one
          my $en_decl = shift (@en_decl);
          for (@en_decl)
            {
              my $decl = &Fxtran::stmt ($_);
              &removeStmt ($decl);
            }

          my @expr = &F ('.//named-E/N/n/text()[' . join (' or ', map { "string(.)=\"$_\"" } @N) . ']', $d1);
          for my $expr (@expr)
            {
              $expr->setData ($N);
            }
           
          my ($ENN) = &F ('./EN-N/N/n/text()', $en_decl);
          $ENN->setData ($N);
          $ENN{$N}++;
        }
    }

  # Remove include of inlined subroutine

  my ($include) = &F ('./include[string(filename)="?"]', lc ($n2) . '.intfb.h', $dp1);

  $include && &removeStmt ($include);

}

=head1 SEE ALSO

L<Fxtran::Generate>, L<Fxtran::Include>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut


1;
