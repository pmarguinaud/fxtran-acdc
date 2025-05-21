package Fxtran::Finder::Include;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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
