#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

use Getopt::Long;
use FileHandle;
use Data::Dumper;
use File::Basename;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::Common;

use Fxtran;
use Fxtran::Outline1;
use Fxtran::Canonic;
use Fxtran::Directive;
use Fxtran::Intrinsic;
use Fxtran::Inline;

my %opts = ();
my @opts_f = qw (inline-contained help);
my @opts_s = qw ();

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

&fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 5000));
&fxtran::setOptions (qw (Statement -line-length 5000));

my $F90 = shift;

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 500 -directive ACDC -canonic)]);

&Fxtran::Canonic::makeCanonic ($d);

&Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

if ($opts{'inline-contained'})
  {
    my @pu = &F ('./object/file/program-unit', $d);
    for my $pu (@pu)
      {
        &Fxtran::Inline::inlineContainedSubroutines ($pu, skipDimensionCheck => 1);
      }
  }

my ($pu) = &F ('./object/file/program-unit', $d);

my ($puName) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);

my $vars = &Fxtran::Outline1::getVariables ($pu);

my @par = &F ('.//parallel-section', $d);

my %parName;

for my $i (0 .. $#par)
  {
    my $par = $par[$i];

    my $parName = $par->getAttribute ('name') || sprintf ('OUTLINE_%3.3d', $i);

    die ("Duplicate section name `$parName'") 
      if ($parName{$parName}++);

    &Fxtran::Outline1::outline ($pu, section => $par, sectionName => $puName. '_' . $parName, variables => $vars);
  }

'FileHandle'->new ('>' . &basename ($F90))->print (&Fxtran::Canonic::indent ($d));

