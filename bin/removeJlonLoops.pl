#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::Common;

use Fxtran;
use Fxtran::Loop;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 300)]);

&Fxtran::Loop::removeJlonLoops ($d, fieldAPI => 1);

'FileHandle'->new (">$f.new")->print ($d->textContent);

