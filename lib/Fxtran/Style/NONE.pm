package Fxtran::Style::NONE;

use strict;

use base qw (Fxtran::Style);

use Fxtran;

sub removeUnusedIncludes
{
  return 0;
}

sub noComputeRoutine
{
  return 0;
}

sub preProcessForOpenACC
{

}

sub customIterator
{
}

sub updateCustomIterator
{
}

sub getActualNproma
{
}

1;
