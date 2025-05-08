#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use local::lib;

use File::Basename;

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use strict;

use Gen;

my $method = &basename ($0, qw (.pl));

if ($method eq 'openacc')
  {
    $method = 'singlecolumn';
  }
elsif ($method eq 'pointerParallel')
  {
    $method = 'parallel';
  }

'Gen'->run ($method, @ARGV);


