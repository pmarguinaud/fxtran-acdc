package Fxtran::IO::arr;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::arr

=head1 DESCRIPTION

FieldAPI class policy module for array-type derived types.  Provides
C<skip> and C<getFieldAPIMember> methods used by C<Fxtran::IO> when
generating I/O code for Fortran derived types that hold pointer
components backed by FieldAPI array objects (names prefixed with
C<F_>).

=cut

use strict;
use Fxtran;
use Data::Dumper;

sub skip
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return unless ($attr->{POINTER});

  return $class->getFieldAPIMember (@_);
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
