package Finder::Directory;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use base qw (Finder::Basic);

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{directory} . '/' . $file;
}


1;
