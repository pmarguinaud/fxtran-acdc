#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Getopt::Long;
use Data::Dumper;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;
use Canonic;
use Fxtran;

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

sub apply
{
  my ($method, $doc) = @_;
  my ($module, $routine) = split (m/::/o, $method);

  eval "use $module;";

  my $c = $@; 
  $c && die ($c);

  {
    no strict 'refs';
    &{$method} ($doc);
  }

}

sub saveSubroutine
{
  my $d = shift;

  my ($F90) = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()', $d, 1);
  $F90 = lc ($F90) . '.F90';

  'FileHandle'->new (">$F90")->print (&Canonic::indent ($d));
  &Fxtran::intfb ($F90);
  return $F90;
}

sub generateSYNCHOST
{
  shift;
  my $d = shift;

  &FieldAPI::makeSyncHost ($d);

  &saveSubroutine ($d);
}

sub generateSYNCDEVICE
{
  shift;
  my $d = shift;

  &FieldAPI::makeSyncDevice ($d);

  &saveSubroutine ($d);
}

sub generatePARALLEL
{
  shift;
  use FieldAPI::Parallel;
  use Stack;
  use Decl;

  my $d = shift;

  return unless (&F ('.//parallel-section', $d));

  &FieldAPI::Parallel::makeParallel ($d, suffix => '_PARALLEL');
  &Decl::changeIntent ($d, 'YDCPG_BNDS', 'INOUT');
  &Stack::addStack ($d, 
                    local => 0,
                    skip => sub 
                    { 
                      my ($proc, $call) = @_; 
                      return ($proc !~ m/_PARALLEL|_SINGLE_COLUMN_/o) || ($proc =~ m/_SYNC_/o); 
                    });

  &saveSubroutine ($d);
}

sub generateFIELDAPIHOST
{
  shift;
  use Subroutine;
  use Call;

  my $d = shift;

  &FieldAPI::pointers2FieldAPIPtrHost ($d);

  &Subroutine::addSuffix ($d, '_FIELD_API_HOST');
  &Call::addSuffix ($d, suffix => '_FIELD_API_HOST');

  &saveSubroutine ($d);
}

sub generateSINGLECOLUMNFIELDAPIHOST
{
  shift;
  use Subroutine;
  use Call;
  use Decl;
  use Stack;
  use Associate;

  my $d = shift;

  my @method = qw (
    Associate::resolveAssociates
    Construct::changeIfStatementsInIfConstructs
    Inline::inlineContainedSubroutines
    FieldAPI::pointers2FieldAPIPtrHost
  );

  for my $method (@method)
    {
      &apply ($method, $d);
    }

  &Loop::removeJlonLoops ($d, fieldAPI => 1);

  my $suffix = '_SINGLE_COLUMN_FIELD_API_HOST';

  &Call::addSuffix ($d, suffix => $suffix);
  &Subroutine::addSuffix ($d, $suffix);
  &Stack::addStack ($d);

  &saveSubroutine ($d);
}

sub generateSINGLECOLUMNFIELDAPIDEVICE
{
  shift;
  use Subroutine;
  use Call;
  use Decl;
  use Stack;
  use Associate;
  use OpenACC;
  use DrHook;

  my $d = shift;

  my @method = qw (
    Associate::resolveAssociates
    Construct::changeIfStatementsInIfConstructs
    Inline::inlineContainedSubroutines
    FieldAPI::pointers2FieldAPIPtrDevice
  );

  for my $method (@method)
    {
      &apply ($method, $d);
    }

  &Loop::removeJlonLoops ($d, fieldAPI => 1);

  my $suffix = '_SINGLE_COLUMN_FIELD_API_DEVICE';

  &Call::addSuffix ($d, suffix => $suffix);
  &Subroutine::addSuffix ($d, $suffix);
  &Stack::addStack ($d);

  &OpenACC::routineSeq ($d);

  &DrHook::remove ($d);
  
  &saveSubroutine ($d);
}

sub preProcessIfNewer
{
  my ($f1, $f2, $opts) = @_;

  if (&newer ($f1, $f2))
    {
      use Directive;
      print "Preprocess $f1\n";

      &copy ($f1, $f2);

      &Fxtran::intfb ($f2);

      my $d = &Fxtran::parse (location => $f1, fopts => [qw (-construct-tag -no-include -line-length 500 -directive ACDC -canonic)]);
      
      &Canonic::makeCanonic ($d);

      &Directive::parseDirectives ($d, name => 'ACDC');

      my @generate = &F ('.//generate', $d);

      for my $generate (&F ('.//generate', $d))
        {
          my @target = split (m,\W,o, $generate->getAttribute ('target'));
          for my $target (@target)
            {
              my $method = "generate$target";
              print "  $method\n";
              __PACKAGE__->$method ($d->cloneNode (1));
            }
        }

    }
}

my @opts_f = qw (update compile external-drhook);
my @opts_s = qw (arch);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);


my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = grep { ! m/\.F90\.xml$/o } map { &basename ($_) } <support/*>;

if ($opts{'external-drhook'})
  {
    @support = grep { $_ !~ m/^(?:abor1|yomhook)\.F90$/o } @support; 
  }

my $dir = "compile.$opts{arch}";

&mkpath ($dir);
chdir ($dir);

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
#           &copyIfNewer ("../compute/$f", $f);
            &preProcessIfNewer ("../compute/$f", $f, \%opts);
          }
     }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("$Bin/Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ('make -j4 wrap_cpg_dia_flux.x') and die;
  }





