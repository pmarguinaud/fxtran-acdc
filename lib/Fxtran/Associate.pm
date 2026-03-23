package Fxtran::Associate;

=head1 NAME

Fxtran::Associate

=head1 DESCRIPTION

This module provides the C<resolveAssociates> function, which replaces associated expressions
with their contents. All C<ASSOCIATE> blocks are scanned and selectors are replaced 
by actual expressions in the block.

The main argument of C<resolveAssociates> is an XML node which maps to a FORTRAN entity
such as a program unit or something smaller like a C<DO> loop or something else.

The C<outer> option allows for processing outer C<ASSOCIATE> blocks (those outside the 
section we are working with). 

C<ASSOCIATE> constructs are removed when the outer option is not active.

=head1 LIMITATIONS

C<ASSOCIATE> selectors other than named expressions are not supported; for instance:

  ASSOCIATE (X => 1 + 2)

C<ASSOCIATE> selectors mapped to complex slices are not supported; for instance:

  TYPE TT
    REAL :: X
  END TYPE

  TYPE (TT) :: YY (N)

  ASSOCIATE (XX => YY%X)
=cut

use FileHandle;
use Data::Dumper;

use strict;

use Fxtran;

sub resolveAssociates
{

=head2 resolveAssociates

Resolve all C<ASSOCIATE> constructs reachable from the given XML node C<$d>.
Each associate selector is inlined - occurrences of the associate name inside
the block are replaced by the associated expression, including any additional
references (array subscripts, component accesses) appended to the name.

When the C<outer> option is set, only C<ASSOCIATE> blocks that are ancestors
of C<$d> (i.e. outside the current scope) are processed and are not removed.
Without C<outer>, the C<ASSOCIATE> construct itself is removed after inlining.

=cut

  my $d = shift;
  my %args = @_;

  my @assoc = $args{outer} ? &F ('ancestor::associate-construct', $d) : &F ('.//associate-construct', $d);

  for my $assoc (@assoc)
    {
      my @as = &F ('./associate-stmt/associate-LT/associate', $assoc);

      # Apply association rules
  
      for my $as (@as)
        {
          my ($n) = &F ('./associate-N/n/text()', $as, 1);
          my ($e) = &F ('./selector/named-E', $as);
  
          my @expr = reverse (&F ('.//named-E[./N/n/text()="?"]', $n, $assoc));
  
          for my $expr (@expr)
            {
              # List of references for current expression
  
              my @r = &F ('./R-LT/node()', $expr);
              my $E = $e->cloneNode (1);
              if (@r)
                {
                  my ($rlt) = &F ('./R-LT', $E);
                  unless ($rlt)
                    {
                      $rlt = &n ('<R-LT/>');
                      $E->appendChild ($rlt);
                    }
  
                  # Append expression references to new expression
                  for (@r)
                    {
                      $rlt->appendChild ($_);
                    }
                }
  
              # Replace expression with its association
  
              $expr->replaceNode ($E);
            }
  
        }
   
      unless ($args{outer})
        {
          # Remove ASSOCIATE block & statements
  
          $assoc->firstChild->unbindNode ();
          $assoc->lastChild->unbindNode ();
  
          for (&F ('./node()', $assoc))
            {
              $assoc->parentNode->insertBefore ($_, $assoc);
            }
  
          $assoc->unbindNode ();
        }
    }
  
  
}


=head1 SEE ALSO

L<Fxtran::Call>, L<Fxtran::Formatter>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut
1;
