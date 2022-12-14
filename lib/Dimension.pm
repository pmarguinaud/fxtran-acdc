package Dimension;

use strict;
use Fxtran;

sub attachArraySpecToEntity
{
  my $d = shift;

  # Remove dimension attributes and attach the array spec to entities

  my @decl = &F ('.//T-decl-stmt[./attribute[string(attribute-N)="DIMENSION"]]', $d);

  for my $decl (@decl)
    {
      my ($dim) = &F ('./attribute[string(attribute-N)="DIMENSION"]', $decl);
      my ($as) = &F ('./array-spec', $dim);
      my @en_decl = &F ('./EN-decl-LT/EN-decl', $decl);
      for my $en_decl (@en_decl)
        {
          next if (&F ('./array-spec', $en_decl));
          my ($N) = &F ('./EN-N', $en_decl);
          $en_decl->insertAfter ($as->cloneNode (1), $N);
        }
      $dim->previousSibling->unbindNode ();
      $dim->unbindNode ();
    }
}

1;
