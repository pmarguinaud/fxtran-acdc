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
use Module;
use Call;
use Canonic;
use DrHook;
use Identifier;
use Cycle49;
use Cycle50;
use Decl;
use Dimension;
use Include;
use Inline;
use Finder;
use Pointer;
use Print;
use Style;

my $SUFFIX = '_OPENACC';

sub addValueAttribute
{
  my $d = shift;

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  my @intent = &F ('./T-decl-stmt'    
                 . '[_T-spec_/intrinsic-T-spec[string(T-N)="REAL" or string(T-N)="INTEGER" or string(T-N)="LOGICAL"]]' # Only REAL/INTEGER/LOGICAL
                 . '[not(.//array-spec)]'                                                                              # Without dimensions
                 . '//attribute[string(intent-spec)="IN"]'                                                             # Only arguments
                 , $dp); 

  for my $intent (@intent)
    {
      for my $x (&t (' '), &n ('<attribute><attribute-N>VALUE</attribute-N></attribute>'), &t (', '))
        {
          $intent->parentNode->insertAfter ($x, $intent);
        }
    }

}

sub updateFile
{
  my ($F90, $code) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$F90"); $fh ? <$fh> : undef };
  
  if ((! defined ($c)) || ($c ne $code))
    {
      unlink ($F90);
      &mkpath (&dirname ($F90));
      my $fh = 'FileHandle'->new (">$F90"); 
      $fh or die ("Cannot write to $F90");
      $fh->print ($code); 
      $fh->close ();
    }
}

sub processSingleModule
{
  my ($d, $find, %opts) = @_;

  my @pu = &F ('./program-unit', $d);

  for my $pu (@pu)
    {
      &processSingleRoutine ($pu, $find, %opts);
    }

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  if ($opts{interfaces})
    {
      my @pu = &F ('./interface-construct/program-unit', $dp);
     
      for my $pu (@pu)
        {
          &processSingleInterface ($pu, $find, %opts);
        }
    }

  &Module::addSuffix ($d, $SUFFIX);
}

sub processSingleInterface
{
  my ($d, $find, %opts) = @_;

  my $end = $d->lastChild;

  &ReDim::reDim ($d, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
  
  if ($opts{'value-attribute'})
    {
      &addValueAttribute ($d);
    }
  
  &Subroutine::addSuffix ($d, $SUFFIX);
  
  &OpenACC::routineSeq ($d);
  
  &Stack::addStack 
  (
    $d, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
  );

}

sub saveDebug
{
  my ($d) = @_;
  
  my ($file, $line) = (caller (0))[1,2];

  $file = &basename ($file);

  my $count = 0;

  my $F90;
  while (1)
    {
      $F90 = "debug/$count.$file:$line.F90";
      last unless (-f $F90);
      $count++;
    }

  (-d 'debug') or mkdir ('debug');

  'FileHandle'->new (">$F90")->print ($d->textContent);
}

sub processSingleRoutine
{
  my ($d, $find, %opts) = @_;

  my @pointer;

  unless ($opts{dummy})
    {

      for my $in (@{ $opts{inlined} })
        {
          my $f90in = $find->resolve (file => $in);
          my $di = &Fxtran::parse (location => $f90in, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)], dir => $opts{tmp});
          &Canonic::makeCanonic ($di);
          &Inline::inlineExternalSubroutine ($d, $di, %opts);
        }
      
      if ($opts{'inline-contained'})
        {
          &Inline::inlineContainedSubroutines ($d, find => $find, inlineDeclarations => 1, comment => $opts{'inline-comment'}, style => $opts{style});
        }
     
      if ($opts{cycle} eq '49')
        {
          &Cycle49::simplify ($d, set => $opts{'set-variables'});
        }
      elsif ($opts{cycle} eq '50')
        {
          &Cycle50::simplify ($d, set => $opts{'set-variables'});
        }
      
      @pointer = &Pointer::setPointersDimensions ($d, 'no-check-pointers-dims' => $opts{'no-check-pointers-dims'})
        if ($opts{pointers});
      
      &Loop::removeNpromaLoops ($d, style => $opts{style}, pointer => \@pointer);
      
      &ReDim::reDim ($d, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
      
      
      if ($opts{'value-attribute'})
        {
          &addValueAttribute ($d);
        }

    }

  &Subroutine::addSuffix ($d, $SUFFIX);
  
  unless ($opts{dummy})
    {
      &Call::addSuffix ($d, suffix => $SUFFIX, match => sub { ! $opts{style}->noComputeRoutine (@_) });
    }
  
  &OpenACC::routineSeq ($d);

  &Stack::addStack 
  (
    $d, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
    pointer => \@pointer,
  );

  if ($opts{dummy})
    {
      &Fxtran::intfb_body ($d->ownerDocument ());
      my ($end) = &F ('./end-subroutine-stmt', $d);  
      my $abort = &s ('CALL ABOR1_ACC ("ERROR : WRONG SETTINGS")');
      $end->parentNode->insertBefore ($_, $end) for ($abort, &t ("\n"));
    }
  else
    {
      &Pointer::handleAssociations ($d, pointers => \@pointer)
        if ($opts{pointers});
      
      &DrHook::remove ($d) unless ($opts{drhook});
      
      &Include::removeUnusedIncludes ($d) 
        if ($opts{style}->removeUnusedIncludes ());
      

      $opts{style}->handleMessages ($d, %opts);

      &Print::useABOR1_ACC ($d);
      &Print::changeWRITEintoPRINT ($d);
      &Print::changePRINT_MSGintoPRINT ($d);
    }


}


my %opts = (cycle => 49, tmp => '.', style => 'MFPHYS');
my @opts_f = qw (help drhook only-if-newer version stdout 
                 modi value-attribute redim-arguments stack84 
                 pointers inline-contained debug interfaces dummy inline-comment interface);
my @opts_s = qw (dir cycle inlined no-check-pointers-dims set-variables files base tmp style);

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

my $suffix = lc ($SUFFIX);
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


my $d = &Fxtran::parse (location => $F90, fopts => [qw (-canonic -construct-tag -no-include -no-cpp -line-length 500)], dir => $opts{tmp});

&Canonic::makeCanonic ($d);

$opts{style} = 'Style'->new (%opts, document => $d);

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
        &processSingleModule ($pu, $find, %opts);
      }
    elsif ($kind eq 'subroutine')
      {
        &processSingleRoutine ($pu, $find, %opts);
      }
    else
      {
        die;
      }
  }


if ($opts{version})
  {
    my $version = &Fxtran::getVersion ();
    my ($file) = &F ('./object/file', $d);
    $file->appendChild (&n ("<C>! $version</C>"));
    $file->appendChild (&t ("\n"));
  }

if ($opts{stdout})
  {
    print &Canonic::indent ($d);
  }
else
  {

    'FileHandle'->new (">$F90out.xml")->print ($d->toString);
    &updateFile ($F90out, &Canonic::indent ($d));


    if ($opts{interface})
      {
        if ($singleRoutine)
          {
            if ($opts{modi})
              {
                &Fxtran::modi ($F90out, $opts{dir});
              }
            else
              {
                &Fxtran::intfb ($F90out, $opts{dir}, $opts{style}->includeExtension ());
              }
          }
      }
  }

