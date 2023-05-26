#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Inline;
use Canonic;
use Decl;

my ($f1, $f2) = @ARGV;

my ($d1, $d2) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } ($f1, $f2);

'FileHandle'->new (">1.F90")->print ($d1->textContent);
'FileHandle'->new (">2.F90")->print ($d2->textContent);
