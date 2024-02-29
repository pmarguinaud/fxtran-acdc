package Pointer;

use strict;
use Fxtran;


sub setPointersDimensions
{
  my $d = shift;
  my %args = @_;

  my %nocheck = map { ($_, 1) } @{ $args{'no-check-pointers-dims'} };

  my @pointer;

  my @en_decl = &F ('.//T-decl-stmt[.//attribute[string(attribute-N)="POINTER"]]//EN-decl', $d);

  for my $en_decl (@en_decl)
    {
      my ($pointer) = &F ('./EN-N', $en_decl, 1);

      my ($as_pointer) = &F ('./array-spec', $en_decl);
      my @assoc = &F ('.//pointer-a-stmt[./E-1[string(.)="?"]][./E-2/named-E[not(R-LT)]', $pointer, $d);

      my $as_pointee0;

      for my $assoc (@assoc)
        {
          my ($pointee) = &F ('./E-2/named-E/N', $assoc, 1);

          my ($as_pointee) = &F ('.//EN-decl[string(EN-N)="?"]/array-spec', $pointee, $d);

          if ($as_pointee0)
            {
              # Check we have the same array spec
              unless ($nocheck{$pointer})
                {
                  die if ($as_pointee0 ne $as_pointee->textContent);
                }
            }
          else
            {
              $as_pointer->replaceNode ($as_pointee->cloneNode (1));
              push @pointer, $pointer;
              $as_pointee0 = $as_pointee->textContent;
            }
        }
    }

  return @pointer;
}

sub handleAssociations
{
  my ($d, %opts) = @_;

  for my $pointer (@{ $opts{pointers} || [] })
    {
      my @assoc = &F ('.//pointer-a-stmt[./E-1[string(.)="?"]][./E-2/named-E[not(R-LT)]', $pointer, $d);
      for my $assoc (@assoc)
        {
          my ($pointee) = &F ('./E-2/named-E/N', $assoc, 1);
          $assoc->replaceNode (&s ("assoc ($pointer, $pointee)"));
        }
      my @nullify = &F ('.//nullify-stmt[./arg-spec/arg/named-E[string(N)="?"]]', $pointer, $d);
      for my $nullify (@nullify)
        {
          $nullify->replaceNode (&s ("nullptr ($pointer)"));
        }
    }
}


1;
