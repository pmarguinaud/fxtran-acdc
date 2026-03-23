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
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  for my $I (@{ $self->{I} })
    {
      return "$I/$file" if (-f "$I/$file");
    }
}

1;
