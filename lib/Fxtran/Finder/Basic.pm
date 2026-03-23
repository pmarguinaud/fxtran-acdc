package Fxtran::Finder::Basic;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::Basic

=head1 DESCRIPTION

Base class for all finder implementations. Provides common logic for locating
interface files, supporting both the ARPEGE/IFS convention (C<.intfb.h> and
C<.h> files) and the MesoNH convention (C<modi_*.F90> files). Concrete
subclasses must implement the C<resolve> method.

=head1 FUNCTIONS

=cut

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

  $intf && (-s $intf) && return $intf;

  # MesoNH

  my $modi = $self->resolve (file => "modi_$name.F90");

  return $modi;
}

1;
