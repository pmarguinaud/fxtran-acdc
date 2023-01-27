#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Stack;
use Associate;
use Loop;
use OpenACC;
use ReDim;
use Construct;
use DIR;
use Subroutine;
use Call;
use Canonic;

my $suffix = '_OPENACC';

my $F90 = shift;
my $dir = shift;

$dir ||= &dirname ($F90);

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-canonic -construct-tag -no-include -no-cpp -line-length 500)]);

&Associate::resolveAssociates ($d);

&Construct::changeIfStatementsInIfConstructs ($d);
&Construct::apply 
($d, 
  '//named-E[string(N)="LMUSCLFA"]',        &e ('.FALSE.'),
  '//named-E[string(.)="YDLDDH%LFLEXDIA"]', &e ('.FALSE.'),
);

&DIR::removeDIR ($d);

&Loop::removeJlonLoops ($d);

&ReDim::reDim ($d);

&Subroutine::addSuffix ($d, $suffix);

&Call::addSuffix ($d, suffix => $suffix);

&OpenACC::routineSeq ($d);

&Stack::addStack ($d);

$suffix = lc ($suffix);

$F90 =~ s/\.F90/$suffix.F90/;
$F90 = $dir . '/' . &basename ($F90);

'FileHandle'->new (">$F90")->print (&Canonic::indent ($d));

&Fxtran::intfb ($F90, $dir);

