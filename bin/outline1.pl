#!/usr/bin/perl -w

use strict;

use Getopt::Long;
use FileHandle;
use Data::Dumper;
use File::Basename;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Outline1;
use Canonic;
use Directive;
use Intrinsic;

my %opts = ();
my @opts_f = qw (help);
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

&Canonic::makeCanonic ($d);

&Directive::parseDirectives ($d, name => 'ACDC');

my ($pu) = &F ('./object/file/program-unit', $d);

my ($puName) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);

my $vars = &Outline1::getVariables ($pu);

my @par = &F ('.//parallel-section', $d);

my %parName;

for my $i (0 .. $#par)
  {
    my $par = $par[$i];

    my $parName = $par->getAttribute ('name') || sprintf ('OUTLINE_%3.3d', $i);

    die ("Duplicate section name `$parName'") 
      if ($parName{$parName}++);

    &Outline1::outline ($pu, section => $par, sectionName => $puName. '_' . $parName, variables => $vars);
  }

'FileHandle'->new ('>' . &basename ($F90))->print (&Canonic::indent ($d));

