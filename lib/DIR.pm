package DIR;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;

sub removeDIR
{
  my $d = shift;

  # !DIR$ 
  # !DEC$

  my @dir = &F ('.//C[starts-with(string(.),"!DIR$") or '
              .      'starts-with(string(.),"!DEC$") or '
              .      'starts-with(string(.),"!NEC$")]', $d);

  for (@dir)
    {   
      $_->unbindNode (); 
    }   

}

1;
