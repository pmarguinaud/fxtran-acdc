package Fxtran::IO::flu;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::flu

=head1 DESCRIPTION

FieldAPI class policy module for flux (FLU) derived types.  Provides
C<skip> and C<getFieldAPIMember> methods used by C<Fxtran::IO> when
generating I/O code.  Components whose names end in C<_B> or that
match C<TYPE_XFU>, C<YXFUPT>, C<TYPE_CFU>, or C<YCFUPT> are always
skipped.

=cut

use strict;
use Fxtran;

sub skip
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  if ($comp =~ m/^(?:\w+_B|TYPE_XFU|YXFUPT|TYPE_CFU|YCFUPT)$/o)
    {
      return 1;
    }

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

=head1 SEE ALSO

L<Fxtran::IO>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
