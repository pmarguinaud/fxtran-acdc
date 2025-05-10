package Fxtran::Finder::Files;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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
