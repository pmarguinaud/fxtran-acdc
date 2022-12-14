#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Inline;

my ($f) = @ARGV;

my $d = &Fxtran::parse (location => $f);

&Inline::inlineContainedSubroutines ($d);

print ($d->textContent ());
#'FileHandle'->new (">$f.new")->print ($d->textContent ());

