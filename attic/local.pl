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

use Fxtran;

sub removeEnDecl
{
  my $en_decl = shift;

  my $lt = $en_decl->parentNode;

  my @n = &F ('following-sibling::node()', $en_decl);
  @n = reverse (&F ('preceding-sibling::node()', $en_decl)) unless (@n);

  $en_decl->unbindNode ();

  for (@n)
    {
      last if ($_->nodeName eq 'EN-decl');
      $_->unbindNode ();
    }
  
  unless ($lt->childNodes ())
    {
      my $stmt = &Fxtran::stmt ($lt);
      $stmt->unbindNode ();
    }
}

my $F90 = shift;

my $d = &parse (location => $F90, fopts => [qw (-construct-tag -no-cpp -line-length 500)]);

my @pu = &F ('.//program-unit', $d);

my ($pu) = @pu;

my %expr = map { ($_, 1) } &F ('.//named-E/N', $pu, 1);

for my $en_decl (&F ('./T-decl-stmt/EN-decl-LT/EN-decl', $pu))
  {

    my ($N) = &F ('./EN-N', $en_decl, 1);

    next if ($expr{$N});

    &removeEnDecl ($en_decl);

  }

$F90 =~ s/\.F90$/.new.F90/o;

'FileHandle'->new (">$F90")->print ($d->textContent);
