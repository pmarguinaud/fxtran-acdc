package Identifier;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;

sub rename
{
  my $d = shift;

  my %map = @_;

  while (my ($k, $v) = each (%map))
    {

      my @expr = &F ('.//named-E[string(N)="?"]/N/n/text()', $k, $d);
     
      for (@expr)
        {   
          $_->setData ($v);
        }   
     
      my @en_decl = &F ('.//EN-N[string(N)="?"]/N/n/text()', $k, $d);
     
      for (@en_decl)
        {   
          $_->setData ($v);
        }   
    }
}


1;
