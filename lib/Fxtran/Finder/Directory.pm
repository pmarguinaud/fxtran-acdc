package Fxtran::Finder::Directory;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::Directory

=head1 DESCRIPTION

Finder implementation that resolves all source files relative to a single
directory. The directory is stored in the C<directory> attribute of the object.
This is the simplest possible finder strategy.

=head1 FUNCTIONS

=cut

use strict;
use base qw (Fxtran::Finder::Basic);

sub resolve
{

=head2 resolve

Resolves a filename (passed as the C<file> named argument) by simply
prepending the configured C<directory> attribute. Returns the resulting path
string; no existence check is performed.

=cut

  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{directory} . '/' . $file;
}


=head1 SEE ALSO

L<Fxtran::Finder>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
