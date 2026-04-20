package Fxtran::IO::sfv;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::sfv

=head1 DESCRIPTION

FieldAPI class policy module for surface variable (SFV) derived types.
Provides C<skip> and C<getFieldAPIMember> methods used by
C<Fxtran::IO> when generating I/O code.  The same tiled-pointer and
group-name components as C<Fxtran::IO::sfc> are skipped.  When
looking up the corresponding FieldAPI member, the leading character of
the component name is stripped before prepending C<F_>.

=cut

use strict;
use Fxtran;
use Data::Dumper;

sub skip
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
 
  substr ($comp, 0, 1, ''); 

  if (my $en_decl = $en_decl_hash->{"F_$comp"})
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($tspec) = &Fxtran::F ('./_T-spec_', $stmt);  
      my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
      return "F_$comp" if ($tname =~ m/^FIELD_/o);
    }
}

=head1 SEE ALSO

L<Fxtran::IO>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
