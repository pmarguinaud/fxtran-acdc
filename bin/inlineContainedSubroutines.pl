#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Inline;
use Canonic;

my ($f) = @ARGV;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);

&Canonic::makeCanonic ($d);

&Inline::inlineContainedSubroutines ($d);

print (&Canonic::indent ($d));
#'FileHandle'->new (">$f.new")->print ($d->textContent ());

