package Fxtran::Ref;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Ref

=head1 DESCRIPTION

Low-level utilities for working with array and function reference nodes in a
fxtran XML parse tree.  Provides C<parensToArrayRef> to convert a
C<parens-R> function-call reference node into an C<array-R> subscript node,
C<resolveParensRef> to apply that conversion throughout a subtree, and
C<getRLT> to retrieve (or create) the C<R-LT> reference-list child of an
expression node.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;

sub parensToArrayRef
{

=head2 parensToArrayRef

Converts a C<parens-R> function-call reference node into an C<array-R>
subscript node in-place by renaming the node and its child list, and wrapping
each element in a C<lower-bound> child so it becomes a proper section subscript.

=cut

  my $r = shift;
  if ($r->nodeName eq 'parens-R')
    {   
      my ($elt) = &F ('./element-LT', $r);

      $r->setNodeName ('array-R');
      $elt->setNodeName ('section-subscript-LT');
      for my $i (&F ('./element', $elt))
        {
          $i->setNodeName ('section-subscript');
          my ($lb) = &n ('<lower-bound/>');
          $lb->appendChild ($_) for ($i->childNodes ());
          $i->appendChild ($lb);
        }
    }   

}

sub resolveParensRef
{

=head2 resolveParensRef

Walks all direct C<parens-R> children of the given node and converts each
ambiguous one (i.e. not a genuine function call whose name starts with C<F>)
to an C<array-R> subscript node via C<parensToArrayRef>.

=cut

  my $d = shift;
  my @r = &F ('./parens-R', $d);

  for my $r (@r)
    {
      my $rlt = $r->parentNode;
      my @r = &F ('./ANY-R', $rlt);
      if (scalar (@r) == 1)
        {
          my $expr = $rlt->parentNode;
          my ($N) = &F ('./N', $expr, 1);
          next if (substr ($N, 0, 1) eq 'F')
        }
      &parensToArrayRef ($r);
    }

}

sub getRLT
{

=head2 getRLT

Returns the C<R-LT> (reference list) child of an expression node, creating
and appending an empty one if it does not already exist.

=cut

  my $expr = shift;

  my ($rlt) = &F ('./R-LT', $expr);

  unless ($rlt)
    {   
      $expr->appendChild ($rlt = &n ('<R-LT/>'));
    }   

  return $rlt;
}


1;
