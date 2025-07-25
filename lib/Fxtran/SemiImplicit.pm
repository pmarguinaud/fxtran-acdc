package Fxtran::SemiImplicit;

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::SingleBlock;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::SingleBlock::processSingleRoutine 
    ($pu, %opts, 'suffix-singleblock' => '_SINGLEBLOCK'); 


  for my $hor (&F ('.//horizontal-section', $pu))
    {
      for my $call (&F ('.//call-stmt', $hor))
        {
          my ($argspec) = &F ('./arg-spec', $call); 
          $argspec->appendChild ($_) for (&t (','), &n ('<arg><arg-N n="LDACC"><k>LDACC</k></arg-N>=' . &e ('LDACC') . '</arg>'));
        }
    }

}

1;
