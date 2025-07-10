package Fxtran::Pointer::Parallel::OpenMP;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Fxtran::Pointer::Parallel;
use Fxtran;

sub getAddBlockIndex
{
  1;
}

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
  my ($par, $t, %opts) = @_;

  my $style = $par->getAttribute ('style');
  $style = $style ? 'Fxtran::Style'->new (style => $style) : $opts{style};

  my @firstprivate = ('YLCPG_BNDS');

  push @firstprivate, $style->customIterator ();

  my %firstprivate = map { ($_, 1) } @firstprivate;

  my @priv = &Fxtran::Pointer::Parallel::getPrivateVariables ($par, $t);
  @priv = grep { ! $firstprivate{$_} } @priv;

  my ($do) = &F ('.//do-construct[./do-stmt[string(do-V)="JBLK"]]', $par);

  my $C = &n ('<C>!$OMP PARALLEL DO PRIVATE (' . join (', ', @priv)  . ') FIRSTPRIVATE (' . join (', ', @firstprivate) . ')</C>');
  
  $do->parentNode->insertBefore ($C, $do);
  $do->parentNode->insertBefore (&t ("\n"), $do);

}

sub makeParallel
{
  shift;
  my ($pu, $par1, $t, %opts) = @_;

  my $style = $par1->getAttribute ('style');
  $style = $style ? 'Fxtran::Style'->new (style => $style) : $opts{style};
  
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


  if ($style->customIterator ())
    {
      my ($update) = &F ('.//call-stmt[string(procedure-designator)="YLCPG_BNDS%UPDATE"]', $do);
      $style->updateCustomIterator ($update);
    }

  $par1->insertBefore ($init, $do);
  $par1->insertBefore (&t ("\n"), $do);

  &setOpenMPDirective ($par1, $t, %opts);

  return $par1;
}


1;
