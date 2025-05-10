package Fxtran::Finder::Pack::Build;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;
use base qw (Fxtran::Finder::Basic);

use Fxtran::Finder::Pack;
use Fxtran::Finder::Include;

sub new
{
  my $class = shift;

  my $self = $class->SUPER::new (@_);

  $self->{pack} = $ENV{TARGET_PACK};
  $self->{pack} = 'File::Spec'->rel2abs ($self->{pack});

  $self->{finderpack} = 'Fxtran::Finder::Pack'->new (pack => $self->{pack});

  $self->{includepack} = 'Fxtran::Finder::Include'->new (@_);

  return $self;
}

sub resolve
{
  my $self = shift;

  my $r;

  if ($r = $self->{includepack}->resolve (@_))
    {
      return $r;
    }
  elsif ($r = $self->{finderpack}->resolve (@_))
    {
      return $r;
    }
}

1;
