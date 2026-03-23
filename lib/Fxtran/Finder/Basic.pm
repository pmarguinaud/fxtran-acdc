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

=head2 new

Constructor. Accepts a hash of keyword arguments and blesses them into the
class. Subclasses typically call C<SUPER::new> and then add their own
initialisation on top.

=cut

  my $class = shift;
  my $self = bless {@_}, $class;
  return $self;
}

sub getInterface
{

=head2 getInterface

Given a subroutine or function C<name> (via the C<name> named argument),
attempts to locate its interface file. First tries the ARPEGE/IFS convention
(C<name.intfb.h>, then C<name.h>); if neither is found it falls back to the
MesoNH convention (C<modi_name.F90>). Returns the resolved path, or undef if
no interface file can be found. Relies on the C<resolve> method implemented by
the concrete subclass.

=cut

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
