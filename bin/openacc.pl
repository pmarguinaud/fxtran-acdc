#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";


use SingleColumn;
use Fxtran;
use Canonic;
use Util;

my %opts = (cycle => 49, tmp => '.', pragma => 'OpenACC', suffix => '_OPENACC');
my @opts_f = qw (help drhook only-if-newer version stdout 
                 value-attribute redim-arguments stack84 merge-interfaces
                 pointers inline-contained interfaces dummy inline-comment interface);
my @opts_s = qw (dir cycle inlined no-check-pointers-dims set-variables files base tmp style pragma suffix);

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
      join ('', map { "  --$_=...\n" } @opts_s) .
     "\n";
    exit (0);
  }


for my $opt (qw (no-check-pointers-dims inlined set-variables))
  {
    $opts{$opt} = [$opts{$opt} ? split (m/,/o, $opts{$opt}) : ()];
  }

my $F90 = shift;

$opts{dir} ||= &dirname ($F90);

my $suffix = lc ($opts{suffix});
(my $F90out = $F90) =~ s/\.F90/$suffix.F90/;
$F90out = $opts{dir} . '/' . &basename ($F90out);


if ($opts{'only-if-newer'})
  {
    my $st = stat ($F90);
    my $stout = stat ($F90out);
    if ($st && $stout)
      {
        exit (0) unless ($st->mtime > $stout->mtime);
      }
  }


my $d = &Fxtran::parse (location => $F90, fopts => [qw (-canonic -construct-tag -no-include -no-cpp -line-length 5000)], dir => $opts{tmp});

&Canonic::makeCanonic ($d);

$opts{style} = 'Style'->new (%opts, document => $d);

$opts{pragma} = 'Pragma'->new (%opts);

my $find = 'Finder'->new (files => $opts{files}, base => $opts{base});

$opts{style}->preProcessForOpenACC ($d, %opts, find => $find);

my @pu = &F ('./object/file/program-unit', $d);

my $singleRoutine = scalar (@pu) == 1;

for my $pu (@pu)
  {
    my $stmt = $pu->firstChild;
    (my $kind = $stmt->nodeName) =~ s/-stmt$//o;
    if ($kind eq 'module')
      {
        $singleRoutine = 0;
        &SingleColumn::processSingleModule ($pu, $find, %opts);
      }
    elsif ($kind eq 'subroutine')
      {
        &SingleColumn::processSingleRoutine ($pu, $find, %opts);
      }
    else
      {
        die;
      }
  }


&Util::addVersion ($d)
  if ($opts{version});

if ($opts{stdout})
  {
    print &Canonic::indent ($d);
  }
else
  {
    &mkpath (&dirname ($F90out));
    'FileHandle'->new (">$F90out.xml")->print ($d->toString);
    &Util::updateFile ($F90out, &Canonic::indent ($d));


    if ($opts{interface} && $singleRoutine)
      {
        $opts{style}->generateInterface ($F90out, %opts);
      }
  }

