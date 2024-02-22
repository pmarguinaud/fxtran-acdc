package Pointer::Parallel::OpenMP;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Pointer::Parallel;
use Fxtran;

sub getDefaultWhere
{
  'host';
}

sub onlySimpleFields
{
  0;
}

sub requireUtilMod
{
  0;
}

sub setOpenMPDirective
{
  my ($par, $t) = @_;

  my $style = $par->getAttribute ('style') || 'ARPIFS';

  my @firstprivate = ('YLCPG_BNDS');

  if ($style eq 'MESONH')
    {
      push @firstprivate, 'D';
    }

  my %firstprivate = map { ($_, 1) } @firstprivate;

  my @priv = &Pointer::Parallel::getPrivateVariables ($par, $t);
  @priv = grep { ! $firstprivate{$_} } @priv;

  my ($do) = &F ('.//do-construct[./do-stmt[string(do-V)="JBLK"]]', $par);

  my $indent = &Fxtran::getIndent ($do);

  my $C = &n ('<C>!$OMP PARALLEL DO PRIVATE (' . join (', ', @priv)  . ') FIRSTPRIVATE (' . join (', ', @firstprivate) . ')</C>');
  
  $do->parentNode->insertBefore ($C, $do);
  $do->parentNode->insertBefore (&t ("\n" . (' ' x $indent)), $do);

}

sub makeParallel
{
  shift;
  my ($par1, $t) = @_;

  my $style = $par1->getAttribute ('style') || 'ARPIFS';
  my $FILTER = $par1->getAttribute ('filter');

  my $init;
  
  if ($FILTER)
    {
      $init = &s ('CALL YLCPG_BNDS%INIT (YL_FGS%KLON, YL_FGS%KGPTOT)');
    }
  else
    {
      $init = &s ('CALL YLCPG_BNDS%INIT (YDCPG_OPTS)');
    }

  my ($do) = &F ('./do-construct', $par1);
  my $indent = &Fxtran::getIndent ($do);

  if ($style eq 'MESONH')
    {
      my ($update) = &F ('.//call-stmt[string(procedure-designator)="YLCPG_BNDS%UPDATE"]', $do);
      my $p = $update->parentNode;

      $p->insertAfter (&s ("D%NIE = YLCPG_BNDS%KFDIA"), $update);
      $p->insertAfter (&t ("\n" . (' ' x $indent)), $update);
      $p->insertAfter (&s ("D%NIB = YLCPG_BNDS%KIDIA"), $update);
      $p->insertAfter (&t ("\n" . (' ' x $indent)), $update);

      $p->insertAfter (&s ("D%NIJE = YLCPG_BNDS%KFDIA"), $update);
      $p->insertAfter (&t ("\n" . (' ' x $indent)), $update);
      $p->insertAfter (&s ("D%NIJB = YLCPG_BNDS%KIDIA"), $update);
      $p->insertAfter (&t ("\n" . (' ' x $indent)), $update);

    }

  $par1->insertBefore ($init, $do);
  $par1->insertBefore (&t ("\n"), $do);

  &setOpenMPDirective ($par1, $t);

  return $par1;
}


1;
