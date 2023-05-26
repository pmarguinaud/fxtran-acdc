#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Data::Dumper;

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Inline;
use Canonic;
use Decl;
use Dimension;

my ($f, @f) = @ARGV;

my ($d, @d) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } ($f, @f);

for ($d, @d)
  {
    &Canonic::makeCanonic ($_);
    &Dimension::attachArraySpecToEntity ($_);
  }

'FileHandle'->new (">$f.old")->print (&Canonic::indent ($d));

for (@d)
  {
    &Inline::inlineExternalSubroutine ($d, $_, skipDimensionCheck => 0);
  }

$d->normalize ();

'FileHandle'->new (">$f.new")->print (&Canonic::indent ($d));

