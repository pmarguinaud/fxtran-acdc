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

use Common;

use Fxtran;
use Inline;
use Canonic;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);

&Canonic::makeCanonic ($d);

&Inline::inlineContainedSubroutines ($d, @ARGV);

#print (&Canonic::indent ($d));
'FileHandle'->new (">$f.new")->print (&Canonic::indent ($d));

