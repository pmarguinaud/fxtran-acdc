package Fxtran::Finder::Files;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::Files

=head1 DESCRIPTION

Finder implementation backed by an explicit list of files. At construction time
the list is indexed by basename so that C<resolve> can look up any file by name
in O(1). Used when the caller supplies a C<files> argument to
L<Fxtran::Finder/new>.

=head1 FUNCTIONS

=cut

use strict;
use base qw (Fxtran::Finder::Basic);

use File::Basename;
use Data::Dumper;

sub new
{
  my $class = shift;
  my %args = @_;
  my $self = bless {}, $class;

  my @files = @{ $args{files} };

  %$self = map { (&basename ($_), $args{base} . '/'. $_) } @files;

  return $self;
}

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{$file};
}

1;
