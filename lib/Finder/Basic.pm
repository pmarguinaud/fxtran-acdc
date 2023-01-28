package Finder::Basic;

use strict;

sub new
{
  my $class = shift;
  my $self = bless {@_}, $class;
  return $self;
}

sub getInterface
{
  my $self = shift;
  my %args = @_;

  my $name = lc ($args{name});

  return $self->resolve (file => "$name.intfb.h") || $self->resolve (file => "$name.h");
}

1;
