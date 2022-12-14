package OpenACC;

use strict;
use Fxtran;
use List::MoreUtils qw (uniq);

sub insertDirective
{
  my ($p, $d, %c)  = @_;

  my $indent = ' ' x &Fxtran::getIndent ($p);

  my $P = $p->parentNode;

  my @d = (" $d ");

  my $N = 5;

  for my $c (sort keys (%c))
    {
      next unless (my @l = sort &uniq (@{ $c{$c} }));
      my $f = 1;
      while (my @x = splice (@l, 0, $N))
        {
          push @d, join (', ', @x); $d[-1] .= ', ' if (scalar (@l));
          if ($f)
            {
              $d[-1] = "$c (" . $d[-1];
              $f = 0;
            }
          else
            {
              $d[-1] = (' ' x (2 + length ($c))) . $d[-1];
            }
          unless (@l)
            {
              $d[-1] = $d[-1] . ") ";
            }
        }
    }
  
  for my $i (0 .. $#d)
    {
      if ($i < $#d)
        {
          $d[$i] = $d[$i] . '&amp;'
        }
      if ($i > 0)
        {
          $d[$i] = '&amp;' . $d[$i];
        }
      $d[$i] = "!\$ACC$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  for my $d (@d)
    {
      $P->insertBefore (&n ("<C>$d</C>"), $p);
      $P->insertBefore (&t ("\n"), $p);
    }


  $P->insertBefore (&t ("\n$indent"), $p);
      
}

sub parallelLoopGang
{
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL LOOP GANG', %c);
}

sub loopVector
{
  my ($p, %c) = @_;
  &insertDirective ($p, 'LOOP VECTOR', %c);
}

sub routineVector
{
  my $d = shift;
  my ($pu) = &F ('./object/file/program-unit', $d);
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $pu, 1); 
  $pu->insertAfter (&n ("<C>!\$acc routine ($N) vector</C>"), $pu->firstChild);
  $pu->insertAfter (&t ("\n"), $pu->firstChild);
}

sub routineSeq
{
  my $d = shift;
  my ($pu) = &F ('./object/file/program-unit', $d);
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $pu, 1); 
  $pu->insertAfter (&n ("<C>!\$acc routine ($N) seq</C>"), $pu->firstChild);
  $pu->insertAfter (&t ("\n"), $pu->firstChild);
}

sub serial
{
  my ($p, %c) = @_;
  &insertDirective ($p, 'SERIAL', %c);
  $p->parentNode->insertAfter (&n ("<C>!\$ACC END SERIAL</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);
}

1;
