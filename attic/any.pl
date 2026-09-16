#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use File::Basename;
use FileHandle;

use FindBin qw ($Bin);
use lib "$Bin/../lib";
use local::lib;

use fxtran;
use fxtran::xpath;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Reduction;
use Fxtran::Style;
use Fxtran::Bt;
use Fxtran::Finder;
use Fxtran::Util;

my $F90 = shift;

my $d = &parse (location => $F90, fopts => [qw (-construct-tag -no-cpp -line-length 500 -canonic)]);

&Fxtran::Canonic::makeCanonic ($d);

my %opts = (jlon => 'JLON', kidia => 'KIDIA', kfdia => 'KFDIA');

&Fxtran::Reduction::reduceAnyIntrinsic ($d, \%opts);

'FileHandle'->new (">any.F90.xml")->print ($d->toString);

my $code = &Fxtran::Canonic::indent ($d);

'FileHandle'->new (">any.F90")->print ($code);
