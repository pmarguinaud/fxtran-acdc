package Finder::Basic;

use strict;
use Data::Dumper;

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

  # ARPEGE/IFS

  my $intf = $self->resolve (file => "$name.intfb.h") || $self->resolve (file => "$name.h");

  $intf && return $intf;

  # MesoNH

  my $modi = $self->resolve (file => "modi_$name.F90");

  return $modi;
}

1;
