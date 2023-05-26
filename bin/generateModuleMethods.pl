#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Fxtran;
use Fxtran::IO;


my %opts = qw (dir .);
my @opts_f = qw (save load copy help);
my @opts_s = qw (dir out tmp);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

if ($opts{help})
  {
    print
     "Usage: " . &basename ($0) . "\n" .
      join ('', map { "  --$_\n" } @opts_f) .
      join ('', map { "  --$_=...\n" } @opts_f) .
     "\n";
    exit (0);
  }


( -d $opts{dir}) or &mkpath ($opts{dir});

$opts{'skip-types'} = sub { };
$opts{'skip-components'} = sub { };

my $F90 = shift;

my $doc = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 800)], dir => $opts{tmp});

if ($opts{load} || $opts{save} || $opts{copy})
  {
    &Fxtran::IO::process_module ($doc, \%opts);
  }

