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
  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  for my $include (&F ('./include', $dp))
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
      next if (&F ('.//call-stmt[string(procedure-designator)="?"]', $name, $ep));
      my $next = $include->nextSibling;
      $next->unbindNode () if ($next->textContent eq "\n");
      $include->unbindNode (); 
    }   
}

sub addInclude
{
  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  my $X = &n ('<C/>');

  if (my ($inc) = &F ('./include', $dp))
    {
      $dp->insertAfter ($X, $inc);
    }
  else
    {
      $dp->appendChild ($X);
    }

  for my $file (@_)
    {
      my $include = &n ('<include>#include "<filename>' . lc ($file) . '</filename>"</include>');
      $dp->insertAfter ($include, $X);
      $dp->insertAfter (&t ("\n"), $X);
    }

  $X->unbindNode ();
}


1;
