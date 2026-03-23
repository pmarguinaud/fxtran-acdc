package Fxtran::Style::SPECTRAL;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::SPECTRAL

=head1 DESCRIPTION

Style class for IFS/Arpege spectral-space routines (horizontal diffusion
and related transforms). This class derives from C<Fxtran::Style::IAL> and
defines the naming conventions for spectral code, where the horizontal loop
iterator is C<JSP>, the loop bounds are C<KSTA> and C<KEND>, and the loop
size (nproma) is one of several C<YDGEOMETRY%YRMP%NSPEC2_*_HD> fields.
The vertical level iterator is C<JLEV>.

The C<matchDocument> method always returns false; spectral-style files are
not auto-detected and must be selected explicitly.

=cut

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
