#!/usr/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Inline;
use Canonic;
use Decl;

my ($f, @f) = @ARGV;

my ($d, @d) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } ($f, @f);

for ($d, @d)
  {
    &Canonic::makeCanonic ($_);
  }

'FileHandle'->new (">$f.old")->print (&Canonic::indent ($d));

for (@d)
  {
    &Inline::inlineExternalSubroutine ($d, $_);
  }

'FileHandle'->new (">$f.new")->print (&Canonic::indent ($d));

