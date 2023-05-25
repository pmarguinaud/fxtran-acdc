package Cycle48;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Construct;

sub simplify
{
  my $d = shift;

  &Construct::changeIfStatementsInIfConstructs ($d);


  &Construct::apply 
  ($d, 
    '//named-E[string(N)="LMUSCLFA"]',                          &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]',                   &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]', &e ('.FALSE.'),
    '//named-E[string(.)="YSPP_CONFIG%LSPP"]',                  &e ('.FALSE.'),
    '//named-E[string(.)="LMCAPEA"]',                           &e ('.FALSE.'),
  );


}

1;
