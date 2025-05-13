package Fxtran::PATH;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use File::Basename;

(my $pm = __PACKAGE__) =~ s,::,/,go;
$pm .= '.pm';

my $TOP = $INC{$pm};

for (1 .. 3)
  {
    $TOP = &dirname ($TOP);
  }

$ENV{PATH} = "$TOP/bin:$ENV{PATH}";



1;
