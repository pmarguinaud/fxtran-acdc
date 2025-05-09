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

use Fxtran::Common;
use Fxtran;
use Fxtran::Inline;
use Fxtran::Canonic;
use Fxtran::Decl;
use Fxtran::Dimension;

my ($f, @f) = @ARGV;

my ($d, @d) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } ($f, @f);

for ($d, @d)
  {
    &Fxtran::Canonic::makeCanonic ($_);
    &Fxtran::Dimension::attachArraySpecToEntity ($_);
  }

'FileHandle'->new (">$f.old")->print (&Fxtran::Canonic::indent ($d));

for (@d)
  {
    &Fxtran::Inline::inlineExternalSubroutine ($d, $_, skipDimensionCheck => 0);
  }

$d->normalize ();

'FileHandle'->new (">$f.new")->print (&Fxtran::Canonic::indent ($d));

