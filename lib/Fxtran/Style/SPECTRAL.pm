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
  die;
}

sub jlon
{
  die;
}

sub jlev
{
  die;
}

sub kidia
{
  die;
}

sub kfdia
{
  die;
}

sub matchDocument
{
  shift;
  my $d = shift;

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KSTA"]', $d)
             || (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KEND"]', $d)
              && &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDGEOMETRY"]', $d)));

  return unless (&F ('./object/file/program-unit/execution-part//do-construct', $d));

  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JSP"]', $d));
  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JLEV"]', $d));

  my @as = &F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-decl/array-spec', $d);

  for my $as (@as)
    {
      my @ss = &F ('./shape-spec-LT/shape-spec', $as, 1);
      return 1 if (($ss[0] =~ m/\bNFLEVL\b/o) && ($ss[1] eq 'KSTA:KEND'));
    }

  return;
}

1;
