package Fxtran::IO::cpg;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::IO::cpg

=head1 DESCRIPTION

FieldAPI class policy module for CPG (column physics grid) derived
types.  Provides C<skip> and C<getFieldAPIMember> methods used by
C<Fxtran::IO> when generating I/O code.  In addition to the standard
pointer check, this module unconditionally skips components named
C<ZVIEW>, C<F_DATA>, and C<ZDATA>.

=cut

use strict;
use Fxtran;
use Data::Dumper;

sub skip
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  my $ret;

  goto RETURN unless ($attr->{POINTER});

  if ($comp =~ m/^(?:ZVIEW|F_DATA|ZDATA)$/o)
    {
      $ret = 1;
      goto RETURN;
    }

  $ret = $class->getFieldAPIMember (@_);

RETURN:

  $ret ||= 0;

  return $ret;
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
