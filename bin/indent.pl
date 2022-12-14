#!/usr/bin/perl -w

use strict;

use FileHandle;
use File::Basename;
use FindBin qw ($Bin);

use lib "$Bin/../lib";

use Common;
use Canonic;
use Fxtran;


for my $f (@ARGV)
  {
    my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 500 -canonic)]);
    print "==> $f <==\n";
    print &Canonic::indent ($d);
  }

