package Fxtran;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use XML::LibXML;
use Data::Dumper;
use Carp qw (croak);

use fxtran;
use fxtran::xpath;

use strict;

use base qw (Exporter);
our @EXPORT = qw (s e F f n t TRUE FALSE);

{

my $version;
sub getVersion
{
  use FindBin qw ($Bin);
  use Cwd;

=pod

commit b9206730bbe52494d44b39b5d3a701868f334be0
Author: Philippe Marguinaud <philippe.marguinaud@meteo.fr>
Date:   Sun Feb 5 16:09:41 2023 +0000

    Add OpenMPSingleColumn transform

=cut

  unless ($version)
    {
      my @log = split (m/\n/o, `/usr/bin/git --git-dir=$Bin/../.git log -n1`);
      ($version) = ($log[0] =~ m/commit\s+(\w+)/o);
    }

  return $version;
}

}

sub removeListElement
{

# Remove element from list, take care of removing comma before or after the element

  my $x = shift;

  my $nn = $x->nodeName;

  my ($p) = $x->parentNode;
  
  my @cf = &F ('following-sibling::text()[contains(.,",")]', $x);   
  my @cp = &F ('preceding-sibling::text()[contains(.,",")]', $x);   
  
  if (@cf)
    {   
      $cf[+0]->unbindNode (); 
    }   
  elsif (@cp)
    {   
      $cp[-1]->unbindNode (); 
    }   
  
  $x->parentNode->appendChild (&t (' '));
  my $l = $x->parentNode->lastChild;
  
  $x->unbindNode (); 
  
  while ($l)
    {   
      last if (($l->nodeName ne '#text') && ($l->nodeName ne 'cnt'));
      $l = $l->previousSibling;
      last unless ($l);
      $l->nextSibling->unbindNode;
    }   

  return &F ("./$nn", $p) ? 0 : 1;
}



sub expand
{
  my $stmt = shift;

  for (&F ('.//cnt', $stmt), &F ('.//C', $stmt))
    {
      $_->unbindNode ();
    }
  for (&f ('.//text ()', $stmt))
    {
      my $data = $_->data;
      if ($data =~ m/\n/o)
        {
          $data =~ s/\s+/ /go;
          $_->setData ($data);
        }
    }

  $stmt->normalize ();
}

# Returns the statement the element belongs to

sub stmt
{
  my $e = shift;
  my @anc = reverse &F ('./ancestor::*', $e);
  my ($stmt) = grep { $_->nodeName =~ m/-stmt$/o } @anc;
  return $stmt;
}

sub expr
{
  my $e = shift;
  my @anc = reverse &f ('./ancestor::*', $e);
  my ($expr) = grep { $_->nodeName =~ m/-E$/o } @anc;
  return $expr;
}

sub TRUE
{
  &n ('<literal-E>.TRUE.</literal-E>');
}

sub FALSE
{
  &n ('<literal-E>.FALSE.</literal-E>');
}

sub stmt_is_executable
{
  my $stmt = shift;
  &croak ("Undefined stmt\n") unless ($stmt);

  my @noexec = ('subroutine-stmt', 'use-stmt', 'T-decl-stmt', 'end-subroutine-stmt', 'data-stmt', 'save-stmt',
                'implicit-none-stmt', 'T-stmt', 'component-decl-stmt', 'end-T-stmt');
  my %noexec = map {($_, 1)} @noexec;

  if ($noexec{$stmt->nodeName})
    {
      return 0;
    }
  return 1;
}


1;
