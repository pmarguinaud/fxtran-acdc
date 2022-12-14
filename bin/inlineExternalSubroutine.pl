#!/usr/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Inline;
use Canonic;

my ($d1, $d2) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } @ARGV;

&Inline::inlineExternalSubroutine ($d1, $d2);

print &Canonic::indent ($d1);

