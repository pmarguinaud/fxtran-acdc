package Fxtran::Style::DYNAMICS;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::DYNAMICS

=head1 DESCRIPTION

Style class for the IFS/Arpege dynamics code. This class derives from
C<Fxtran::Style::IAL> and defines the naming conventions used in dynamics
routines, where the horizontal loop iterator is C<JROF>, the loop bounds
are C<KST> and C<KEND>, and the loop size (nproma) may be given by
C<KPROMA>, C<YDCPG_OPTS%KLON>, or one of the C<YDGEOMETRY%YRDIM%NPROMA*>
fields.

The C<matchDocument> method identifies dynamics source files by the
presence of the C<KST> dummy argument or local declarations of C<JROF>
or C<YDCPG_SL1>.

=cut

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
