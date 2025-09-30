package Fxtran::Pointer;

=head1 NAME

Fxtran::Pointer

=head1 SYNOPSIS

  my @p = &setPointersDimensions ($pu); # Set pointers dimensions and return list of pointers

  ... # Single-column transform

  &handleAssociations ($pu, pointers => \@p); # Process pointer association statements & nullify statements

=head1 DESCRIPTION

This module provides utilities to manage pointer variables, and transform them to 
CRAY pointers.

=head1 FUNCTIONS

=cut

use strict;

use Fxtran;

sub setPointersDimensions
{

=head2 setPointersDimensions

Look for pointer variables associated to regular FORTRAN arrays and replace implicit 
shapes array specs with full fledged dimensions (dimensions are taken from pointees).

Return the list of pointer variables.

=cut

  my $d = shift;
  my %args = @_;

  my %nocheck = map { ($_, 1) } @{ $args{'no-check-pointers-dims'} };

  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  my @pointer;
 
  # Look for variables with the POINTER attribute

  my @en_decl = &F ('./T-decl-stmt[.//attribute[string(attribute-N)="POINTER"]]//EN-decl', $dp);

  for my $en_decl (@en_decl)
    {
      my ($pointer) = &F ('./EN-N', $en_decl, 1);

      my ($as_pointer) = &F ('./array-spec', $en_decl);

      # Find pointer association involving this variable (ie X => ...)
 
      my @assoc = &F ('.//pointer-a-stmt[./E-1[string(.)="?"]][./E-2/named-E[not(R-LT)]', $pointer, $ep);

      # Guess variable shape from pointee

      my $as_pointee0;

      for my $assoc (@assoc)
        {
          my ($pointee) = &F ('./E-2/named-E/N', $assoc, 1);

          my ($as_pointee) = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[string(EN-N)="?"]/array-spec', $pointee, $dp);

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
              # Replace variable array spec (:,:) with pointee array spec
              $as_pointer->replaceNode ($as_pointee->cloneNode (1));
              push @pointer, $pointer;
              $as_pointee0 = $as_pointee->textContent;  # First pointee
            }
        }
    }

  # Return list of pointer variables identified (those we managed to guess the dimensions)

  return @pointer;
}

sub handleAssociations
{

=head2 handleAssociations

Replace associations with fxtran macros. Replace C<NULLIFY> statements with fxtran macros.

=cut

  my ($d, %opts) = @_;

  my ($ep) = &F ('./execution-part', $d);

  for my $pointer (@{ $opts{'pointers'} || [] })
    {
      # Associations involving the pointer variable (X => Y)

      for my $assoc (&F ('.//pointer-a-stmt[./E-1[string(.)="?"]][./E-2/named-E[not(R-LT)]', $pointer, $ep))
        {
          # Use fxtran association
          my ($pointee) = &F ('./E-2/named-E/N', $assoc, 1);
          $assoc->replaceNode (&s ("fxtran_acdc_assoc ($pointer, $pointee)"));
        }

      # Nullify statements involving the pointer variable

      for my $arg (&F ('.//nullify-stmt/arg-spec/arg/named-E[string(N)="?"]', $pointer, $ep))
        {
          my $argspec = $arg->parentNode->parentNode;
          my $nullify = $argspec->parentNode;
          my @arglist = &F ('./arg', $argspec);

          my $nullptr = &s ("fxtran_acdc_nullptr ($pointer)");

          $nullify->parentNode->insertAfter ($_, $nullify) for ($nullptr, &t ("\n"));

          if ($arg->nextSibling)  # X, 
            {
              $arg->nextSibling->unbindNode ();
            }
          elsif ($arg->previousSibling) # , X
            {
              $arg->previousSibling->unbindNode ();
            }
          else
            {
              $nullify->unbindNode ();
            }

          $arg->unbindNode ();
        }
    }
}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 SEE ALSO

=head1 COPYRIGHT

Meteo-France 2025

=cut


1;
