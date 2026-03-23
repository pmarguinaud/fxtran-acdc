package Fxtran::Finder::Include;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::Include

=head1 DESCRIPTION

Finder implementation that searches for files through a list of include
directories, similar to a C preprocessor include path. Directories are passed
via the C<I> constructor argument (with or without a leading C<-I> prefix).
The current directory C<.> is always prepended to the search path. The first
directory that contains the requested file wins.

=head1 FUNCTIONS

=cut

use strict;
use base qw (Fxtran::Finder::Basic);

sub new
{

=head2 new

Constructor. Accepts an C<I> array-ref of include directories (each entry may
optionally carry a leading C<-I> prefix, which is stripped). The current
directory C<.> is always prepended to the resulting search path.

=cut

  my $class = shift;
  my %args = @_;
  my $self = bless \%args, $class;
  for (@{ $self->{I} })
    {
      s/^-I//o;
    }
  unshift (@{ $self->{I} }, '.');
  return $self;
}

sub resolve
{

=head2 resolve

Searches each directory in the include path (C<I> list) in order and returns
the first path C<I/file> for which the file actually exists on disk. The
C<file> argument is passed as a named parameter. Returns undef if the file is
not found in any of the include directories.

=cut

  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  for my $I (@{ $self->{I} })
    {
      return "$I/$file" if (-f "$I/$file");
    }
}

1;
