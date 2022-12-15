#!/usr/bin/perl -w

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

for ($d1, $d2)
  {
    &Canonic::makeCanonic ($_);
  }

'FileHandle'->new (">$f1.old")->print (&Canonic::indent ($d1));
'FileHandle'->new (">$f2.old")->print (&Canonic::indent ($d2));

&Inline::inlineExternalSubroutine ($d1, $d2);

'FileHandle'->new (">$f1.new")->print (&Canonic::indent ($d1));

