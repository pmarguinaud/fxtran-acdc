package Fxtran::Style::SEMIIMPLICIT;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Style::SEMIIMPLICIT

=head1 DESCRIPTION

Style class for the IFS/Arpege semi-implicit solver routines. This class
derives from C<Fxtran::Style::IAL> and defines the naming conventions used
in semi-implicit spectral-space code, where the horizontal loop iterator is
C<JSP>, the loop bounds are C<KSTA> and C<KEND>, and the loop size (nproma)
is one of several C<YDGEOMETRY%YRMP%NSPEC2V_*_SI> fields or C<KSPEC2V>.
The vertical level iterator is C<JLEV>.

The C<matchDocument> method identifies semi-implicit source files by the
presence of array declarations dimensioned by C<KSPEC2V> or
C<YDGEOMETRY%YRMP%NSPEC2V_SI>.

=cut

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

=head1 SEE ALSO

L<Fxtran::Style>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
