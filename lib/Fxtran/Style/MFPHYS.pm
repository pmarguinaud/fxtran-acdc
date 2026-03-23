package Fxtran::Style::MFPHYS;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::MFPHYS

=head1 DESCRIPTION

Style class for Meteo-France physics routines. This class derives from
C<Fxtran::Style::IAL> and defines the naming conventions used in
Meteo-France physics code, where the horizontal loop iterator is C<JLON>,
the loop bounds are C<KIDIA> and C<KFDIA>, and the loop size (nproma) is
C<KLON> or C<YDGEOMETRY%YRDIM%NPROMA>. The vertical level iterator is
C<JLEV>.

The C<matchDocument> method identifies MFPHYS source files by the presence
of a C<KLON> dummy argument (or the combination of C<KIDIA>, C<KFDIA> and
C<YDGEOMETRY>) together with a C<JLON> local variable declaration.

=cut

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
