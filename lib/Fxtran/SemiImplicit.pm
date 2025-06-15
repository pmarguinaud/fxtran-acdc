package Fxtran::SemiImplicit;

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::SingleBlock;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::SingleBlock::processSingleRoutine 
    ($pu, %opts, 'suffix-singleblock' => '_SINGLEBLOCK'); #, 'suffix-singlecolumn' => '_OPENACC');

}

1;
