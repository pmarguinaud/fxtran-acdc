package Finder::Directory;

use strict;

sub new
{
  my $class = shift;
  my $self = bless {@_}, $class;
  return $self;
}

sub resolve
{
  my $self = shift;
  return $self->{directory} . '/' . $_[0];
}


1;
