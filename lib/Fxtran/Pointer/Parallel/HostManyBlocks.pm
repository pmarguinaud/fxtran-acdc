package Fxtran::Pointer::Parallel::HostManyBlocks;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use strict;

use base qw (Fxtran::Pointer::Parallel::ManyBlocks);

BEGIN
{
  print STDERR sprintf ("%s is deprecated, use Fxtran::Pointer::Parallel::ManyBlocks instead\n", __PACKAGE__);
}

1;
