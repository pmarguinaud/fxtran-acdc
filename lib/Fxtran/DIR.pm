package Fxtran::DIR;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::DIR

=head1 DESCRIPTION

Handles compiler-specific directive comments in Fortran source trees.
Provides a function to remove C<!DIR$>, C<!DEC$>, and C<!NEC$> directive
comments from a parsed document.

=head1 FUNCTIONS

=cut

use strict;
use Fxtran;

sub removeDIR
{

=head2 removeDIR

Removes all C<!DIR$>, C<!DEC$>, and C<!NEC$> compiler-specific directive
comments from the parsed document.

=cut

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
