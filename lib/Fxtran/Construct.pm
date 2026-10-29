package Fxtran::Construct;

=head1 NAME

Fxtran::Construct

=head1 DESCRIPTION

This module provides functions whose purpose is construct reorganization.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;
use Data::Dumper;
use FileHandle;
use File::Basename;
use Fxtran;


sub changeIfStatementsInIfConstructs
{

=head2 changeIfStatementsInIfConstructs

Change statements such as:

  IF (LLCOND) CALL SUBR (...)

into:

  IF (LLCOND) THEN
    CALL SUBR (...)
  ENDIF

=cut

  my $d = shift;

  # Change if statements into if constructs

  my @if_stmt = &F ('.//if-stmt', $d);

  for my $if_stmt (@if_stmt)
    {
      my ($action) = &F ('./action-stmt', $if_stmt);
      my ($stmt) = &F ('./ANY-stmt', $action);

      die $if_stmt->textContent unless ($stmt);

      next if ($stmt->nodeName eq 'call-stmt' && $stmt->textContent =~ m/DR_HOOK/o);

      $action->unbindNode ();

      $stmt = $stmt->textContent;
      my $if = $if_stmt->textContent;

      my ($if_construct) = &Fxtran::parse (fragment => << "EOF", fopts => [qw (-line-length 800 -construct-tag)]);
$if THEN
$stmt
ENDIF
EOF

     $if_stmt->replaceNode ($if_construct);
    }

}


sub simplify
{

=head2 simplify

Constant-fold a single expression node C<$e1> within its parent expression.
Handles arithmetic (C<+>), comparison (C<==> / C</=>), and logical (C<.AND.>,
C<.OR.>, C<.NOT.>) operators with literal operands, and removes redundant
parentheses. Recursively simplifies the result if it is a literal.

=cut

  my $e1 = shift;
  my $e = $e1->parentNode;
  return unless ($e->nodeName =~ m/-E$/o);
  
  my $nn;

  if (my ($op) = &F ('./op', $e, 1))
    {
      my ($e2) = grep { $_->unique_key != $e1->unique_key } &F ('./ANY-E', $e);

      my ($i1, $i2);
      my ($r1, $r2);

      ($i1) =       ($e1->textContent =~ m/^(\d+)$/o);
      ($i2) = $e2 ? ($e2->textContent =~ m/^(\d+)$/o) : ();

      ($r1) =       ($e1->textContent =~ m/^(\d*\.\d*)(?:_\w+)?$/o);
      ($r2) = $e2 ? ($e2->textContent =~ m/^(\d*\.\d*)(?:_\w+)?$/o) : ();

      $i1 = $r1 if (defined ($r1));
      $i2 = $r2 if (defined ($r2));

      if (defined ($i1) && $e2 && defined ($i2))
        {
          if (defined ($i1) && defined ($i2))
            {
              if ($op eq '+')
                {
                  my $s = $i1 + $i2;
                  $nn = &n ("<literal-E><l>$s</l></literal-E>");
                }
              elsif (($op eq '==') || ($op eq '.EQ.'))
                {
                  $nn = &e ($i1 == $i2 ? '.TRUE.' : '.FALSE.');
                }
              elsif (($op eq '/=') || ($op eq '.NE.'))
                {
                  $nn = &e ($i1 == $i2 ? '.FALSE.' : '.TRUE.');
                }
            }
        }
      elsif ($e1->textContent eq '.TRUE.')
        {
          if ($op eq '.AND.')
            {
              $nn = $e2;
            }
          elsif ($op eq '.OR.')
            {
              $nn = $e1;
            }
          elsif ($op eq '.NOT.')
            {
              $nn = &n ('<literal-E>.FALSE.</literal-E>');
            }
        }
      if ($e1->textContent eq '.FALSE.')
        {
          if ($op eq '.AND.')
            {
              $nn = $e1;
            }
          elsif ($op eq '.OR.')
            {
              $nn = $e2;
            }
          elsif ($op eq '.NOT.')
            {
              $nn = &n ('<literal-E>.TRUE.</literal-E>');
            }
        }
    }
  elsif ($e->nodeName eq 'parens-E')
    {
      $nn = $e1;
    }

  unless ($nn)
     {
#      print $e->textContent, "\n";
       return;
     }

  $e->replaceNode ($nn); 

  if ($nn->textContent =~ m/^\.(?:TRUE|FALSE)\.$/o)
    {
      &simplify ($nn);
    }

  if ($nn->textContent =~ m/^\d+$/o)
    {
      &simplify ($nn);
    }

}

sub apply
{
  my ($d, %c) = @_;

=head2 apply

Replace expressions with constants values, and simplify the code (ie remove
some C<IF> blocks) when possible.

For instance, a block such as:

  IF (LLCOND1) THEN
    [ list of statements 1 ]
  ELSEIF (LLCOND2) THEN
    [ list of statements 2 ]
  ELSE
    [ list of statements 3 ]
  ENDIF

would become, after forcing C<LLCOND2> to C<.FALSE.>.

  IF (LLCOND1) THEN
    [ list of statements 1 ]
  ELSE
    [ list of statements 3 ]
  ENDIF

This is achieved by calling C<apply> like this:

  &Fxtran::Construct::apply 
  (
    $d,                                 # section to be reorganized
    '//named-E[string(N)="LLCOND2"]',   # XPath expression for expression matching
    &e ('.FALSE.),                      # expression to replace matching expressions with
  );

=cut

  while (my ($k, $v) = each (%c))
    {
      my @expr = &F ($k, $d);
      for my $expr (@expr)
        {
          my $p = $expr->parentNode ();
          next if ($p->nodeName eq 'E-1');
          $expr->replaceNode ($v->cloneNode (1));
        } 
    }

  my @tr = &F ('.//named-E[string(N)="TRIM"][./R-LT/parens-R/element-LT/element/string-E]', $d);
  
  for my $tr (@tr)
    {
      my ($s) = &F ('./R-LT/parens-R/element-LT/element/string-E', $tr);
      $tr->replaceNode ($s);
    }
  
  my @sc = &F ('.//op-E[string(op)="=="][count(./string-E)=2]', $d);

  for my $sc (@sc)
    {
      my ($e1, $e2) = &F ('./string-E/S', $sc, 2);
      for ($e1, $e2)
        {
          s/^["']//o;
          s/["']$//o;
        }
      if ($e1 eq $e2)
        {
          $sc->replaceNode (&n ("<literal-E>.TRUE.</literal-E>"));
        }
      else
        {
          $sc->replaceNode (&n ("<literal-E>.FALSE.</literal-E>"));
        }
    }



  my @expr = &F ('.//literal-E', $d);

  for my $expr (@expr)
    {
      next unless ($expr->textContent =~ m/^(?:\.TRUE\.|\.FALSE\.|\d+|\d*\.\d*(?:_\w+)?)$/o);
      next unless (&Fxtran::stmt ($expr));
      &simplify ($expr);
    }

  for my $ce (&F ('.//if-stmt/condition-E[string(.)=".TRUE." or string(.)=".FALSE."]', $d))
    {
      my $stmt = $ce->parentNode;
      if ($stmt->nodeName eq 'if-stmt')
        {
          if ($ce->textContent eq '.TRUE.')
            {
              my ($stmt1) = &F ('./action-stmt/ANY-stmt', $stmt);
              $stmt->replaceNode ($stmt1);
            }
          else
            {
              $stmt->unbindNode ();
            }
        }
    }

  &simplifyIfConstructs ($d);
}

sub simplifyIfConstructs
{

=head2 simplifyIfConstructs

Remove dead branches from C<IF> constructs whose conditions have been reduced to
literal C<.TRUE.> or C<.FALSE.>:

=over 4

=item

blocks with a C<.FALSE.> condition are removed;

=item

blocks following a block with a C<.TRUE.> condition are removed, and this block
becomes the C<ELSE> block (or the only block);

=item

a construct left with no block is removed; a construct left with a single unconditional
block (C<.TRUE.> condition, or C<ELSE>) is replaced by the statements of this block.

=back

The C<END IF> statement is kept, with the name of the construct and its label; when
it is removed, its label is kept on a C<CONTINUE> statement.

=cut

  my $d = shift;

  my @if_construct = &F ('.//if-construct[./if-block/ANY-stmt/condition-E[string(.)=".TRUE." or string(.)=".FALSE."]]', $d);

  # Inner constructs first
  for my $if_construct (reverse (@if_construct))
    {
      my @if_block = &F ('./if-block', $if_construct);

      my ($name) = &F ('./if-then-stmt/N', $if_block[0], 1);

      # Detach the END IF statement, with its label

      my ($end) = &F ('./end-if-stmt', $if_block[-1]);
      my @end = ($end);

      my $prev = $end->previousSibling;
      $prev = $prev->previousSibling if ($prev && ($prev->nodeName eq '#text') && ($prev->data !~ m/\n/o));
      my $label = ($prev && ($prev->nodeName eq 'label')) ? $prev : undef;

      if ($label)
        {
          my $n = $label;
          while (($n = $n->nextSibling) && (! $n->isSameNode ($end)))
            {
              push @end, $n;
            }
          unshift @end, $label;
        }

      $_->unbindNode () for (@end);

      # Blocks which are kept

      my @keep;

      for my $if_block (@if_block)
        {
          my ($stmt) = &F ('./ANY-stmt[1]', $if_block);
          my ($ce) = &F ('./condition-E', $stmt);
          my $cond = $ce ? $ce->textContent : 'ELSE';
          next if ($cond eq '.FALSE.');
          push @keep, [$if_block, $stmt, $cond];
          last if (($cond eq '.TRUE.') || ($cond eq 'ELSE'));
        }

      my %keep = map { ($_->[0]->unique_key, 1) } @keep;

      $_->unbindNode () for (grep { ! $keep{$_->unique_key} } @if_block);

      if ((! @keep) || ($keep[0][2] eq '.TRUE.') || ($keep[0][2] eq 'ELSE'))
        {
          # Replace the construct by the statements of its single unconditional block (if any)

          my @node;

          if (@keep)
            {
              my ($if_block, $stmt) = @{ $keep[0] };
              $stmt->unbindNode ();
              @node = $if_block->childNodes ();
              shift (@node) while (@node && ($node[0]->nodeName eq '#text') && ($node[0]->data !~ m/\S/o)
                                  && ($node[0]->data !~ m/\n/o));
            }

          if ($label)
            {
              push @node, $label, &t (' '), &s ('CONTINUE');
            }

          my $p = $if_construct->parentNode;
          $p->insertBefore ($_, $if_construct) for (@node);

          $if_construct->unbindNode ();

          next;
        }

      # First block left: IF (...) THEN

      if ($keep[0][1]->nodeName ne 'if-then-stmt')
        {
          my ($ce) = &F ('./condition-E', $keep[0][1]);
          my $if = &n ('<if-then-stmt>' . ($name ? "<N><n>$name</n></N>:" : '') . 'IF (</if-then-stmt>');
          $if->appendChild ($ce->cloneNode (1));
          $if->appendChild (&t (') THEN'));
          $keep[0][1]->replaceNode ($if);
        }

      # Last block left, with a .TRUE. condition: ELSE

      if ((@keep > 1) && ($keep[-1][2] eq '.TRUE.'))
        {
          $keep[-1][1]->replaceNode (&n ('<else-stmt>ELSE' . ($name ? " <N><n>$name</n></N>" : '') . '</else-stmt>'));
        }

      $keep[-1][0]->appendChild ($_) for (@end);
    }
}

sub removeEmptyDoConstructs
{

=head2 removeEmptyDoConstructs

Remove C<DO> constructs that contain no statements other than the C<DO> and
C<ENDDO> delimiters. Empty C<IF> constructs nested inside each C<DO> are
removed first.

=cut

  my $d = shift;

  for my $do_construct (reverse (&F ('.//do-construct', $d)))
    {
      &removeEmptyIfConstructs ($do_construct);
      my @stmt = &F ('.//ANY-stmt', $do_construct);
      next unless (scalar (@stmt) == 2);
      $do_construct->unbindNode ();
    }
}

sub removeEmptyIfConstructs
{

=head2 removeEmptyIfConstructs

Remove C<IF> constructs where every block contains only its delimiter
statement (i.e. there are no executable statements inside any branch).

=cut

  my $d = shift;

  for my $if_construct (reverse (&F ('.//if-construct', $d)))
    {
#     &removeEmptyDoConstructs ($if_construct);
      my @if_block = &F ('./if-block', $if_construct);
      my @stmt = &F ('.//ANY-stmt', $if_construct);

      # Non final blocks have a single statement, final block has two statements
      next unless (scalar (@stmt) == (scalar (@if_block) + 1));
      $if_construct->unbindNode ();
    }
}

sub removeEmptyConstructs
{

=head2 removeEmptyConstructs

Convenience wrapper that removes both empty C<IF> constructs and empty C<DO>
constructs from the given document or program unit.

=cut

  my $d = shift;
  &removeEmptyIfConstructs ($d);
  &removeEmptyDoConstructs ($d);
}

=head1 SEE ALSO

L<Fxtran::Decl>, L<Fxtran::Loop>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;
