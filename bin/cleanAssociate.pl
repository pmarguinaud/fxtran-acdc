#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use File::Basename;
use FileHandle;

use FindBin qw ($Bin);
use lib "$Bin/../lib";
use local::lib;

use fxtran;
use fxtran::xpath;

sub indent
{
  my ($code, $indent) = @_;

  for ($code)
    {
      s/\n//goms;
      s/^\s*//o;
      s/\s*$//o;
    }

  my $str = '';

  my $len = -1;
  my $max = 120;
  my $ind = $indent;
  my $pp = sub
  {
    if ($len < 0)
      {
        $str .= $ind;
        $len = 0;
      }
    if ($len + length ($_[0]) > $max)
      {
        $str .= "&\n$ind  & ";
        $len = 0;
      }
    $str .= $_[0];
    $len += length ($_[0]);
  };

  my ($stmt) = &parse (fragment => "$code\nEND ASSOCIATE\n", fopts => [qw (-line-length 10000 -canonic)]);

  my @assoc = &F ('.//associate', $stmt);

  $pp->("ASSOCIATE (");
 
  for my $i (0 .. $#assoc)
    {
      my $assoc = $assoc[$i]->textContent;
      $assoc = "$assoc, " unless ($i == $#assoc);
      $pp->($assoc);
    }

  $pp->(")");
  
  ($stmt) = &parse (fragment => "$str\nEND ASSOCIATE\n", fopts => [qw (-line-length 10000)]);

  return $stmt;
}
my $F90 = shift;

my $doc = &parse (location => $F90, fopts => [qw (-construct-tag -no-cpp -line-length 500)]);

for my $block (reverse (&F ('.//associate-construct', $doc)))
  {

    my @N = &F ('.//named-E/N', $block, 1);
    my %N = map { ($_, 1) } @N;

    my $stmt = $block->firstChild;
    my ($stmt1) = &parse (fragment => $stmt->textContent . "\nEND ASSOCIATE\n", fopts => [qw (-line-length 1000 -canonic)]);

    my $count = 0;

    for my $assoc (&F ('./associate-LT/associate', $stmt1))
      {
        my ($N) = &F ('./associate-N', $assoc, 1);
        next if ($N{$N});

        $count++;

        my ($p, $n) =  ($assoc->previousSibling, $assoc->nextSibling);

        if ($p)
          {
            $p->unbindNode;
            $assoc->unbindNode;
          }
        elsif ($n)
          {
            $n->unbindNode;
            $assoc->unbindNode;
          }
        else
          {
            # Remove ASSOCIATE block
            $block->firstChild->unbindNode;
            $block->lastChild->unbindNode;
            $count = 0;
            my @n = $block->childNodes;
            my $p = $block->parentNode;
            for my $n (@n)
              {
                $p->insertBefore ($n, $block);
              }
            $block->unbindNode;
            last;
          }
      }

    if ($count)
      {
        print "Remove $count ASSOCIATE\n";
        $stmt1 = &indent ($stmt1->textContent, '');
        $stmt->replaceNode ($stmt1);
      }

  }

'FileHandle'->new ('>' . &basename ($F90))->print ($doc->textContent);
