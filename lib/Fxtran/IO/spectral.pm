package Fxtran::IO::spectral;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::spectral

=head1 DESCRIPTION

FieldAPI class policy module for spectral derived types.  Provides
C<skip> and C<getFieldAPIMember> methods used by C<Fxtran::IO> when
generating I/O code.  Pointer components that do not start with C<F_>
are skipped, as are components matching C<SP\dD> (spectral
dimensioned arrays).

=cut

use strict;
use Fxtran;
use Data::Dumper;

sub skip
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return 1 if ($attr->{POINTER} && ! ($comp =~ m/^F_/o));

  return 1 if ($comp =~ m/^SP\dD$/o);
}

sub getFieldAPIMember
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return unless ($attr->{POINTER});

  if (my $en_decl = $en_decl_hash->{"F_$comp"})
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($tspec) = &Fxtran::F ('./_T-spec_', $stmt);  
      my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
      return "F_$comp" if ($tname =~ m/^FIELD_/o);
    }
}

1;
