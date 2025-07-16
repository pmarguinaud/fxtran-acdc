package Fxtran::SemiImplicit;

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::SingleBlock;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

print $opts{style}, "\n";

  &Fxtran::SingleBlock::processSingleRoutine 
    ($pu, %opts, 'suffix-singleblock' => '_SINGLEBLOCK'); 

}

1;
