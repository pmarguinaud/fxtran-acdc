package Fxtran::Pragma::OpenMPTarget;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

use base qw (Fxtran::Pragma);

use Fxtran;
use List::MoreUtils qw (uniq);

sub insertDirective
{
  my ($p, $d, %c)  = @_;

  my $P = $p->parentNode;

  my @d = (" $d ");

  for my $c (sort keys (%c))
    {
      next unless (my @l = sort &uniq (@{ $c{$c} }));
      my $f = 1;

      while (@l)
        {
          my @x;

          while (my $l = shift (@l))
            {
              push @x, $l;
              last if (length (join ('', @x)) > 50);
            }

          push @d, join (', ', @x); $d[-1] .= ', ' if (scalar (@l));
          if ($f)
            {
              if ($c eq 'CREATE')
                {$d[-1] = "MAP(ALLOC:" . $d[-1];}
              elsif ($c eq 'PRESENT')
                {$d[-1] = "MAP(PRESENT:" . $d[-1];}
##                {$d[-1] = "MAP(PRESENT:" . $d[-1];}
##nvidia ???                {$d[-1] = "MAP(TO:" . $d[-1];}
              else
                {$d[-1] = "$c (" . $d[-1];}
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
      $d[$i] = "!\$OMP$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  for my $d (@d)
    {
      $P->insertBefore (&n ("<C>$d</C>"), $p);
      $P->insertBefore (&t ("\n"), $p);
    }


  $P->insertBefore (&t ("\n"), $p);
      
}

sub insertParallelLoopGang
{
  shift;
  my ($p, %c) = @_;
  delete $c{PRESENT};
  $c{THREAD_LIMIT} = delete $c{VECTOR_LENGTH};
  &insertDirective ($p, 'TARGET TEAMS DISTRIBUTE', %c);
}

sub insertParallelLoopGangVector
{
  shift;
  my ($p, %c) = @_;
 &insertDirective ($p, 'TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD', %c);
}

sub insertData
{
  shift;
  my ($p, %c) = @_;
 &insertDirective ($p, 'TARGET DATA ', %c);
 $p->parentNode->insertAfter (&t ("\n"), $p);
 $p->parentNode->insertAfter (&n ("<C>!\$OMP END TARGET DATA</C>"), $p);
 $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub insertLoopVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL DO SIMD', %c);
}

sub insertRoutineVector
{
  shift;
  die;
}

sub insertRoutineSeq
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  $d->insertAfter (&n ("<C>!\$OMP DECLARE TARGET</C>"), $d->firstChild);
  $d->insertAfter (&t ("\n"), $d->firstChild);
}

sub insertSerial
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'TARGET', %c);
  $p->parentNode->insertAfter (&n ("<C>!\$OMP END TARGET</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);

###  &insertDirective ($p, 'TARGET TEAMS PARALLEL NUM_THREADS (1)', %c);
###  $p->parentNode->insertAfter (&n ("<C>!\$OMP END TEAMS</C>"), $p);
###  $p->parentNode->insertAfter (&t ("\n"), $p);
###  $p->parentNode->insertAfter (&n ("<C>!\$OMP END PARALLEL</C>"), $p);
###  $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub enterDataCreate
{
  shift;
  return @_ ? '!$OMP TARGET ENTER DATA MAP (ALLOC: ' . join (', ', @_) . ')' : '';
}

sub exitDataDelete
{
  shift;
  return @_ ? '!$OMP TARGET EXIT DATA MAP (DELETE: ' . join (', ', @_) . ')' : '';
}

sub updateDevice
{
  shift;
  return @_ ? '!$OMP TARGET UPDATE TO (' . join (', ', @_) . ')' : '';
}

sub enterDataAttach
{
  shift;
  return @_ ? '!$OMP TARGET ENTER DATA MAP (TO: ' .  join (', ', @_) . ')' : '';
}

sub exitDataDetach
{
  shift;
  return @_ ? '!$OMP TARGET EXIT DATA MAP (RELEASE: ' .  join (', ', @_) . ')' : '';
}

sub useModule
{
  return 'USE OMP_LIB';
}

sub exprIsPresent
{
  shift;
  my $expr = shift;
  return "OMP_TARGET_IS_PRESENT ($expr, OMP_GET_DEFAULT_DEVICE ())";
}



1;
