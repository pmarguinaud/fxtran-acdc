package Fxtran::Style::MFPHYS;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Style::IAL);
use Fxtran;

use strict;

sub nproma
{
  return qw (KLON YDGEOMETRY%YRDIM%NPROMA);
}

sub kidia
{
  return 'KIDIA';
}

sub kfdia
{
  return 'KFDIA';
}

sub jlon
{
  return 'JLON';
}

sub jlev
{
  return 'JLEV';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JLON");
}

sub matchDocument
{
  shift;
  my $d = shift;

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KLON"]', $d)
             || (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KIDIA"]', $d)
              && &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KFDIA"]', $d)
              && &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDGEOMETRY"]', $d)));

  return 1 unless (&F ('./object/file/program-unit/execution-part//do-construct', $d));

  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JLON"]', $d));

  return 1;
}

1;
