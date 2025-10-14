package Fxtran::Pointer::Parallel::ManyBlocks;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use strict;

use base qw (Fxtran::Pointer::Parallel::BasicManyBlocks);

use Fxtran::Pointer::Parallel;
use Fxtran;

sub getAddBlockIndex
{
  0;
}

sub getDefaultWhere
{
  'host';
}

sub onlySimpleFields
{
  0;
}

sub requireUtilMod
{
  0;
}

sub makeParallel
{
  my $class = shift;
  my ($pu, $par1, $t, %opts) = @_;
  return $class->SUPER::makeParallel ($pu, $par1, $t, %opts, acc => 0);
}

1;
