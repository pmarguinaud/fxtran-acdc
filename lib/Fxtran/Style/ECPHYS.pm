package Fxtran::Style::ECPHYS;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::ECPHYS

=head1 DESCRIPTION

Style class for ECMWF/IFS physics routines. This class derives from
C<Fxtran::Style::IAL> and defines the naming conventions used in IFS
physics code, where the horizontal loop iterator is C<JL>, the loop
bounds are C<KIDIA> and C<KFDIA>, and the loop size (nproma) is C<KLON>
or C<YDGEOMETRY%YRDIM%NPROMA> or C<YDCPG_OPTS%KLON>. The vertical level
iterator is C<JK>.

The C<matchDocument> method identifies ECPHYS source files by the
presence of a C<KLON> dummy argument and a C<JL> local variable
declaration (without C<JLON>).

=cut

use base qw (Fxtran::Style::IAL);
use Fxtran;

use strict;

sub nproma
{
  return qw (KLON YDGEOMETRY%YRDIM%NPROMA YDCPG_OPTS%KLON);
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
  return 'JL';
}

sub jlev
{
  return 'JK';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JL");
}

sub matchDocument
{
  shift;
  my $d = shift;

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KLON"]', $d));

  return 1 unless (&F ('./object/file/program-unit/execution-part//do-construct', $d));

  return if (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JLON"]', $d));

  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JL"]', $d));

  return 1;
}

=head1 SEE ALSO

L<Fxtran::Style>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
