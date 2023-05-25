package Include;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Scope;


sub removeUnusedIncludes
{
  my $doc = shift;
  for my $include (&F ('.//include', $doc))
    {   
      my ($filename) = &F ('./filename', $include, 2); 
      my $name = $filename;
      next if ($name =~ m/\.func\.h$/o);
      next if ($name eq 'stack.h');
      for ($name)
        {
           s/\.intfb\.h$//o;
           s/\.h$//o;
        }
      $name = uc ($name);
      next if (&F ('.//call-stmt[string(procedure-designator)="?"]', $name, $doc));
      my $next = $include->nextSibling;
      $next->unbindNode () if ($next->textContent eq "\n");
      $include->unbindNode (); 
    }   
}

sub addInclude
{
  my $doc = shift;

  my ($x) = &F ('.//include', $doc);
  unless ($x)
    {
      $x = &Scope::getNoExec ($doc);
    }

  for my $file (@_)
    {
      my $include = &n ('<include>#include "<filename>' . lc ($file) . '</filename>"</include>');
      $x->parentNode->insertAfter ($include, $x);
      $x->parentNode->insertAfter (&t ("\n"), $x);
    }
  
}


1;
