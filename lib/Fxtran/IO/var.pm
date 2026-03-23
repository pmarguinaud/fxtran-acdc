package Fxtran::IO::var;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::var

=head1 DESCRIPTION

FieldAPI class policy module for variable (VAR) derived types.
Provides C<skip> and C<getFieldAPIMember> methods used by
C<Fxtran::IO> when generating I/O code.  Only pointer components are
examined; the special component C<P> is mapped to C<T0> before
looking for the corresponding FieldAPI member (using the C<F> prefix
rather than C<F_>).

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

  if ($comp eq 'P')
    {
      $comp = 'T0';
    }

  if (my $en_decl = $en_decl_hash->{"F$comp"})
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($tspec) = &Fxtran::F ('./_T-spec_', $stmt);  
      my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
      return "F$comp" if ($tname =~ m/^FIELD_/o);
    }
}

1;
