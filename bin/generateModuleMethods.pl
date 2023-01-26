#!/usr/bin/perl -w

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

&GetOptions
(
  'dir=s' => \$opts{dir}, 'out=s' => \$opts{out},
  save => \$opts{save}, load => \$opts{load}, copy => \$opts{copy},
  'tmp=s' => \$opts{tmp},
);

( -d $opts{dir}) or &mkpath ($opts{dir});

$opts{'skip-types'} = sub { };
$opts{'skip-components'} = sub { };

my $F90 = shift;

my $doc = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 800)], dir => $opts{tmp});

if ($opts{load} || $opts{save} || $opts{copy})
  {
    &Fxtran::IO::process_module ($doc, \%opts);
  }

