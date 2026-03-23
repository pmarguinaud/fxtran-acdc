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
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{directory} . '/' . $file;
}


1;
