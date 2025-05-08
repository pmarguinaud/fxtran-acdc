package SingleColumn;

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

use Common;
use Fxtran;
use Stack;
use Associate;
use Loop;
use Pragma;
use ReDim;
use Construct;
use DIR;
use Subroutine;
use Module;
use Call;
use Canonic;
use DrHook;
use Identifier;
use Cycle;
use Decl;
use Dimension;
use Include;
use Inline;
use Finder;
use Pointer;
use Print;
use Style;

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

  &Module::addSuffix ($d, $opts{suffix});
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
  
  &Subroutine::addSuffix ($d, $opts{suffix});
  
  $opts{pragma}->insertRoutineSeq ($d);
  
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
     
      'Cycle'->simplify ($d, %opts);
      
      @pointer = &Pointer::setPointersDimensions ($d, 'no-check-pointers-dims' => $opts{'no-check-pointers-dims'})
        if ($opts{pointers});
      
      &Loop::removeNpromaLoops ($d, style => $opts{style}, pointer => \@pointer);
      
      &ReDim::reDim ($d, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
      
      
      if ($opts{'value-attribute'})
        {
          &addValueAttribute ($d);
        }

    }

  &Subroutine::addSuffix ($d, $opts{suffix});
  
  unless ($opts{dummy})
    {
      &Call::addSuffix 
      (
        $d, 
        suffix => $opts{suffix}, 
        match => sub { ! $opts{style}->noComputeRoutine (@_) },
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
    }
  
  $opts{pragma}->insertRoutineSeq ($d);

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
      

      unless ($opts{'merge-interfaces'})
        {
          &Include::removeUnusedIncludes ($d) 
            if ($opts{style}->removeUnusedIncludes ());
        }

      $opts{style}->handleMessages ($d, %opts);

      &Print::useABOR1_ACC ($d);
      &Print::changeWRITEintoPRINT ($d);
      &Print::changePRINT_MSGintoPRINT ($d);
    }


}

1;
