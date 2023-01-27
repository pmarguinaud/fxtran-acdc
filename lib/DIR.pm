package DIR;

use strict;
use Fxtran;

sub removeDIR
{
  my $d = shift;

  # !DIR$ 
  # !DEC$

  my @dir = &F ('.//C[starts-with(string(.),"!DIR$") or starts-with(string(.),"!DEC$")]', $d);

  for (@dir)
    {   
      $_->unbindNode (); 
    }   

}

1;
