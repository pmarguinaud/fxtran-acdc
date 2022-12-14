#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;

&Fxtran::intfb ($_) for (@ARGV);
