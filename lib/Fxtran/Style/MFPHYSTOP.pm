package Fxtran::Style::MFPHYSTOP;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::MFPHYSTOP

=head1 DESCRIPTION

Style class for the top-level Meteo-France physics interface routines.
This class derives from C<Fxtran::Style::MFPHYS> and overrides the loop
size (nproma) and loop bounds to use the C<YDCPG_OPTS> and C<YDCPG_BNDS>
structures that appear at the top of the Meteo-France physics call tree.
Specifically, nproma is taken from C<YDGEOMETRY%YRDIM%NPROMA>,
C<YDGEOMETRY%YRDIM%NPROMNH>, or C<YDCPG_OPTS%KLON>, and the loop bounds
are C<YDCPG_BNDS%KIDIA> and C<YDCPG_BNDS%KFDIA>.

The C<matchDocument> method identifies MFPHYSTOP source files by the
presence of C<YDMF_PHYS>, C<YDMF_PHYS_OUT>, or the combination of
C<YDCPG_OPTS> and C<YDCPG_BNDS> dummy arguments together with a C<JLON>
local variable declaration.

=cut

use base qw (Fxtran::Style::MFPHYS);
use Fxtran;
use Data::Dumper;

use strict;

sub nproma
{
  return qw (YDGEOMETRY%YRDIM%NPROMA YDGEOMETRY%YRDIM%NPROMNH YDCPG_OPTS%KLON);
}

sub kidia
{
  return 'YDCPG_BNDS%KIDIA';
}

sub kfdia
{
  return 'YDCPG_BNDS%KFDIA';
}

sub matchDocument
{
  shift;
  my $d = shift;

  return 1 if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDMF_PHYS"]', $d));

  return 1 if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDMF_PHYS_OUT"]', $d));

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDCPG_OPTS"]', $d));

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDCPG_BNDS"]', $d));

  return 1 unless (&F ('./object/file/program-unit/execution-part//do-construct', $d));

  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JLON"]', $d));

  return 1;
}

1;
