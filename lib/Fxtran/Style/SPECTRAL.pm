package Fxtran::Style::SPECTRAL;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use base qw (Fxtran::Style::IAL);
use Fxtran;

use strict;

sub nproma
{
 return qw (
 YDGEOMETRY%YRMP%NSPEC2_HD
 YDGEOMETRY%YRMP%NSPEC2_I_HD
 YDGEOMETRY%YRMP%NSPEC2_L_HD
 YDGEOMETRY%YRMP%NSPEC2_NH_HD
 YDGEOMETRY%YRMP%NSPEC2_NHX_HD
 YDGEOMETRY%YRMP%NSPEC2_O3_HD
 YDGEOMETRY%YRMP%NSPEC2_Q_HD
 );
}

sub jlon
{
  return 'JSP';
}

sub jlev
{
  return 'JLEV';
}

sub kidia
{
  return 'KSTA';
}

sub kfdia
{
  return 'KEND';
}

sub matchDocument
{
  shift;
  my $d = shift;
  return;
}

1;
