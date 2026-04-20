package Fxtran::Scope;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Scope

=head1 DESCRIPTION

Navigation and whitespace-management utilities for fxtran XML parse trees.
C<getExec> returns the first executable statement or construct in a subtree.
C<getNoExec> returns the last non-executable node that precedes the first
executable statement, which is useful as an insertion point for new
declarations or directives.  C<removeWhiteSpaces> and C<removeWhiteLines>
collapse redundant whitespace text nodes in the tree.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;

sub getExec
{

=head2 getExec

Returns the first executable statement or construct node found in the subtree
rooted at C<$d>, walking up through ancestor construct and statement nodes so
that the returned node is a top-level executable unit.

=cut

  my $d = shift;
 
  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);

  my @anc = &F ('ancestor::*', $exec);

  for my $anc (reverse (@anc))
    {
      if ($anc->nodeName =~ m/-(?:construct|stmt)$/o)
        {
          $exec = $anc;
        }
    }

  return $exec;
}

sub getNoExec
{

=head2 getNoExec

Returns the last non-executable node that immediately precedes the first
executable statement or ACC directive in the subtree.  Useful as an insertion
point when adding declarations or directives without disturbing the executable
section.

=cut

  my $d = shift;
 
  my @stmt = &F ('.//ANY-stmt|.//acc', $d); 

  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) || ($_->nodeName eq 'acc') } @stmt;

  if (! $exec)
    {
      return $stmt[-2];
    }

  my @prev = &F ('preceding::*', $exec);

  my $prev;

  for my $p (reverse (@prev))
    {
      my $nn = $p->nodeName;
      next if ($nn eq '#text');
      next if ($nn eq 'C');
      if ($nn eq 'ACDC-directive')
        {
          next if ($p->textContent =~ m/^PARALLEL/o);
        }
      next if ($nn eq 'ACDC');
      $prev = $p;
      last;
    }

  $prev or die $d->textContent;

  my @anc = &F ('ancestor::*', $prev);

  for my $anc (reverse (@anc))
    {
      my $nn = $anc->nodeName;
      if (($nn =~ m/-(?:construct|stmt)$/o) || ($nn eq 'include'))
        {
          $prev = $anc;
        }
    }

  return $prev;
}

sub removeWhiteSpaces
{

=head2 removeWhiteSpaces

Collapses runs of whitespace-only text nodes (spaces and non-newline
characters) in the subtree to a single newline, normalising the tree after
structural modifications.

=cut

  my $d = shift;

  if ($d->isa ('XML::LibXML::Document'))
    {
      $d = $d->documentElement;
    }

  $d->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $d);

  for my $text (@text)
    {    
      if ($text->data =~ m/\n/goms)
        {
          $text->setData ("\n");
        }
    }    
}

sub removeWhiteLines
{

=head2 removeWhiteLines

Removes consecutive blank lines from whitespace-only text nodes, reducing
each run of multiple newlines to a single newline while preserving indentation
on the last line.

=cut

  my $d = shift;

  if ($d->isa ('XML::LibXML::Document'))
    {
      $d = $d->documentElement;
    }

  $d->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $d);

  for my $text (@text)
    {    
      my $tt = $text->data;
      if ($tt =~ m/\n/goms)
        {
          $tt =~ s/^.*\n([ ]*?)$/\n$1/goms;
          $text->setData ($tt);
        }
    }    
}


=head1 SEE ALSO

L<Fxtran::Decl>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
