package Identifier;

use strict;

sub rename
{
  my $d = shift;

  my %map;

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
