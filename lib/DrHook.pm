package DrHook;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;

sub remove
{
  my $d = shift;

  for my $use (&F ('.//use-stmt[string(module-N)="YOMHOOK"]', $d)) 
    {
      $use->unbindNode ();
    }

  for my $call (&F ('.//if-stmt[.//call-stmt[string(procedure-designator)="DR_HOOK"]]', $d)) 
    {
      $call->unbindNode ();
    }

  for my $decl (&F ('.//T-decl-stmt[.//EN-decl[starts-with(string(EN-N),"ZHOOK_HANDLE")]]', $d))
    {
      $decl->unbindNode ();
    }
}

1;
