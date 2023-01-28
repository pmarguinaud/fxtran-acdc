package Include;

use strict;
use Fxtran;


sub removeUnusedIncludes
{
  my $doc = shift;
  for my $include (&F ('.//include', $doc))
    {   
      my ($filename) = &F ('./filename', $include, 2); 
      (my $name = $filename) =~ s/\.intfb.h$//o;
      $name = uc ($name);
      next if (&F ('.//call-stmt[string(procedure-designator)="?"]', $name, $doc));
      my $next = $include->nextSibling;
      $next->unbindNode () if ($next->textContent eq "\n");
      $include->unbindNode (); 
    }   
}


1;
