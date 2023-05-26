#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Data::Dumper;
use FileHandle;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Decl;


my $F90 = shift;

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -no-cpp -line-length 800)]);

my @args = &F ('.//dummy-arg-LT/arg-N', $d, 1);
my %args = map { ($_, 1) } @args;

&Decl::forceSingleDecl ($d);

for my $en_decl (&F ('.//EN-decl', $d))
  {
    my ($N) = &F ('./EN-N', $en_decl, 1);
    next if ($args{$N});
    next if (my @expr = &F ('.//named-E[string(N)="?"]', $N, $d));
    my $stmt = &Fxtran::stmt ($en_decl);
    $stmt->unbindNode ();
  }

'FileHandle'->new (">$F90.new")->print ($d->textContent);


