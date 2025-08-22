package Fxtran::Style::SEMIIMPLICIT;

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
  return qw (KSPEC2V KSTA:KEND YDGEOMETRY%YRMP%NSPEC2V_VOR_SI YDGEOMETRY%YRMP%NSPEC2V_SI 
             YDGEOMETRY%YRMP%NSPEC2V_NH_SI YDGEOMETRY%YRMP%NSPEC2V_VOR_DDH_SI YDGEOMETRY%YRMP%NSPEC2V_DDH_SI 
             YDGEOMETRY%YRMP%NSPEC2V_NH_DDH_SI YDGEOMETRY%YRMP%NSPEC2V_PA_SI);
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

  my ($dp) = &F ('./object/file/program-unit/specification-part/declaration-part', $d);

  if (&F ('./T-decl-stmt/EN-decl-LT/EN-decl/array-spec/shape-spec-LT' 
        . '[string(shape-spec[1])="KSPEC2V" or string(shape-spec[1])="YDGEOMETRY%YRMP%NSPEC2V_SI"]', 
          $dp))
    {
      return 1;
    }
}

1;
