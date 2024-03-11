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
    my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 500 -canonic -no-cpp -construct-tag)]);
    print "==> $f <==\n";
    print &Canonic::indent ($d);
  }

