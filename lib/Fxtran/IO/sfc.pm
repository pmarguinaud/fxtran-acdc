package Fxtran::IO::sfc;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::sfc

=head1 DESCRIPTION

FieldAPI class policy module for surface (SFC) derived types.
Provides C<skip> and C<getFieldAPIMember> methods used by
C<Fxtran::IO> when generating I/O code.  Components matching
C<P.*_T[019]> (tiled pointer arrays) or named C<F_GROUP>,
C<VARIABLE_GROUP>, or C<PGROUP> are unconditionally skipped.

=cut

use strict;
use Fxtran;
use Data::Dumper;

sub  skip
{ 
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_; 
  return 1 if ($comp =~ m/^P.*_T[019]$/o);
  return 1 if ($comp =~ m/^(?:F_GROUP|VARIABLE_GROUP|PGROUP)$/o);

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
