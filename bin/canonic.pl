#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use FileHandle;
use File::Basename;
use FindBin qw ($Bin);

use lib "$Bin/../lib";

use Common;
use Canonic;

use Fxtran;

for my $f (@ARGV)
  {
    my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 500)]);
    'FileHandle'->new ('>' . &basename ($f) . '.xml')->print ($d->toString);
    &Canonic::makeCanonic ($d);
    'FileHandle'->new ('>canonic.' . &basename ($f) . '.xml')->print ($d->toString);
  }


