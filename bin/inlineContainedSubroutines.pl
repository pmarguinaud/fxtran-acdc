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
use Fxtran::Inline;
use Fxtran::Canonic;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);

&Fxtran::Canonic::makeCanonic ($d);

&Fxtran::Inline::inlineContainedSubroutines ($d, @ARGV);

#print (&Fxtran::Canonic::indent ($d));
'FileHandle'->new (">$f.new")->print (&Fxtran::Canonic::indent ($d));

