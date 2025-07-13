package Fxtran::Style::DYNAMICS;

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
  return qw (KPROMA YDCPG_OPTS%KLON YDGEOMETRY%YRDIM%NPROMA YDGEOMETRY%YRDIM%NPROMNH);
}

sub kidia
{
  return 'KST';
}

sub kfdia
{
  return 'KEND';
}

sub jlon
{
  return 'JROF';
}

sub jlev
{
  return 'JLEV';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JROF");
}

sub matchDocument
{
  shift;
  my $d = shift;


  if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KST"]', $d))
    {
      return 1 unless (&F ('.//do-construct/do-stmt[string(do-V)!="JLEV" and string(do-V)!="JGFL"]', $d));
      return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JROF"]', $d));
    }
  else
    {
      return 1 if (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JROF"]', $d));
      return 1 if (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="YDCPG_SL1"]', $d));
    }
}

1;
