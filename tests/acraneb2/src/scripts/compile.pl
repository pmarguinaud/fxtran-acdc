#!/usr/bin/perl -w

use strict;
use local::lib;
use FindBin qw ($Bin);
use lib "$Bin/../../../../lib";
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Data::Dumper;
use Getopt::Long;
use Bt;
use PATH;
use Compare;


my %opts;

sub newer
{
  my ($f1, $f2)  = @_;
  die unless (-f $f1);
  return 1 unless (-f $f2);
  return stat ($f1)->mtime > stat ($f2)->mtime;
}

sub copyIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Copy $f1 to $f2\n"; 
      &copy ($f1, $f2); 
    }
}

sub saveToFile
{
  my ($x, $f) = @_;

  unless (-d (my $d = &dirname ($f)))
    {
      &mkpath ($d);
    }

  'FileHandle'->new (">$f")->print ($x->textContent ());
  'FileHandle'->new (">$f.xml")->print ($x->toString ());
}

sub addSeqDirective
{
  my $d = shift;
  my ($pu) = &F ('./object/file/program-unit', $d);
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);
  $pu->insertAfter (&n ("<C>!\$acc routine ($N) seq</C>"), $pu->firstChild);
  $pu->insertAfter (&t ("\n"), $pu->firstChild);
}

sub preProcessIfNewer
{
  use Inline;
  use Associate;
  use Fxtran;
  use Stack;
  use Loop;
  use ReDim;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";

      my $d = &Fxtran::parse (location => $f1);
      &saveToFile ($d, "tmp/$f2");

      &Associate::resolveAssociates ($d);
      &saveToFile ($d, "tmp/resolveAssociates/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      &Loop::removeJlonLoops ($d);
      &saveToFile ($d, "tmp/removeJlonLoops/$f2");

      &ReDim::reDim ($d);
      &saveToFile ($d, "tmp/reDim/$f2");

      &addSeqDirective ($d);

      &Stack::addStack ($d);
      &saveToFile ($d, "tmp/addStack/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

      &Fxtran::intfb ($f2);
    }
}

my @opts_f = qw (update compile compare compare-prompt);
my @opts_s = qw (arch);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);

my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = map { &basename ($_) } <support/*>;

&mkpath ("compile.$opts{arch}");

chdir ("compile.$opts{arch}");

if ($opts{update})
  {
    for my $f (@support)
      {
        &copyIfNewer ("../support/$f", $f);
      }
    
    if ($opts{arch} =~ m/^gpu/o)
      {
        for my $f (@compute)
          {
            &preProcessIfNewer ("../compute/$f", $f);
          }
      }
   else
     {
        for my $f (@compute)
          {
            &copyIfNewer ("../compute/$f", $f);
          }
     }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ('make -j4 main.x') and die;
  }

if ($opts{compare})
  {
    &Compare::compare ("../compare.$opts{arch}", "../compile.$opts{arch}", %opts);
  }



