package Pointer::Parallel::OpenMP;

use strict;
use Pointer::Parallel;
use Fxtran;

sub makeParallel
{
  shift;
  my ($par, $t) = @_;
  &Pointer::Parallel::setOpenMPDirective ($par, $t);
  return $par;
}


1;
