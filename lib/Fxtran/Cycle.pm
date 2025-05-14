package Fxtran::Cycle;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

sub simplify
{
  shift;

  my ($d, %opts) = @_;
  return unless (my $cycle = $opts{cycle});

  my $class = 'Fxtran::Cycle' . $cycle;

  eval "use $class";
  my $c = $@;

  die ($c) if ($c);

  $class->simplify ($d, %opts);
}

1;
