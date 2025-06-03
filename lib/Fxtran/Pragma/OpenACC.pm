package Fxtran::Pragma::OpenACC;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Pragma);

use strict;
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


  $P->insertBefore (&t ("\n"), $p);
      
}

sub insertParallelLoopGang
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL LOOP GANG', %c);
}

sub insertParallelLoopGangVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL LOOP GANG VECTOR', %c);
}

sub insertData
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'DATA', %c);
  $p->parentNode->insertAfter (&t ("\n"), $p);
  $p->parentNode->insertAfter (&n ("<C>!\$ACC END DATA</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub insertLoopVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'LOOP VECTOR', %c);
}

sub insertRoutineVector
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  $d->insertAfter (&n ("<C>!\$ACC ROUTINE ($N) VECTOR</C>"), $d->firstChild);
  $d->insertAfter (&t ("\n"), $d->firstChild);
}

sub insertRoutineSeq
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  $d->insertAfter (&n ("<C>!\$ACC ROUTINE ($N) SEQ</C>"), $d->firstChild);
  $d->insertAfter (&t ("\n"), $d->firstChild);
}

sub insertSerial
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'SERIAL', %c);
  $p->parentNode->insertAfter (&n ("<C>!\$ACC END SERIAL</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub enterDataCreate
{
  shift;
  return @_ ? '!$ACC ENTER DATA CREATE (' . join (', ', @_) . ')' : '';
}

sub exitDataDelete
{
  shift;
  return @_ ? '!$ACC EXIT DATA DELETE (' . join (', ', @_) . ')' : '';
}

sub updateDevice
{
  shift;
  return @_ ? '!$ACC UPDATE DEVICE (' . join (', ', @_) . ')' : '';
}

sub enterDataAttach
{
  shift;
  return @_ ? '!$ACC ENTER DATA ATTACH (' .  join (', ', @_) . ')' : '';
}

sub exitDataDetach
{
  shift;
  return @_ ? '!$ACC EXIT DATA DETACH (' .  join (', ', @_) . ')' : '';
}

1;
