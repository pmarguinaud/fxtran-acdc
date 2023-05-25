package PATH;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use File::Basename;

my $TOP = $INC{__PACKAGE__ . '.pm'};

for (1 .. 2)
  {
    $TOP = &dirname ($TOP);
  }

$ENV{PATH} = "$TOP/bin:$ENV{PATH}";



1;
