package Fxtran::SingleColumn;

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

use Fxtran::Common;
use Fxtran;
use Fxtran::Stack;
use Fxtran::Associate;
use Fxtran::Loop;
use Fxtran::Pragma;
use Fxtran::ReDim;
use Fxtran::Construct;
use Fxtran::DIR;
use Fxtran::Subroutine;
use Fxtran::Module;
use Fxtran::Call;
use Fxtran::Canonic;
use Fxtran::DrHook;
use Fxtran::Identifier;
use Fxtran::Decl;
use Fxtran::Dimension;
use Fxtran::Include;
use Fxtran::Inline;
use Fxtran::Finder;
use Fxtran::Pointer;
use Fxtran::Print;
use Fxtran::Style;
use Fxtran::Interface;

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
  my ($d, %opts) = @_;

  my $find = $opts{find};

  my @pu = &F ('./program-unit', $d);

  for my $pu (@pu)
    {
      &processSingleRoutine ($pu, %opts);
    }

  my ($dp) = &F ('./specification-part/declaration-part', $d);

  if ($opts{'process-interfaces'})
    {
      my @pu = &F ('./interface-construct/program-unit', $dp);
     
      for my $pu (@pu)
        {
          &processSingleInterface ($pu, %opts);
        }
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-singlecolumn'});
}

sub processSingleInterface
{
  my ($d, %opts) = @_;

  my $find = $opts{find};

  my $end = $d->lastChild;

  &Fxtran::ReDim::reDim ($d, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
  
  if ($opts{'value-attribute'})
    {
      &addValueAttribute ($d);
    }
  
  &Fxtran::Subroutine::addSuffix ($d, $opts{'suffix-singlecolumn'});
  
  $opts{pragma}->insertRoutineSeq ($d);
  
  &Fxtran::Stack::addStack 
  (
    $d, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
  );

}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  my $find = $opts{find};

  my @pointer;

  unless ($opts{dummy})
    {

      for my $in (@{ $opts{inlined} })
        {
          my $f90in = $find->resolve (file => $in);
          my $di = &Fxtran::parse (location => $f90in, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)], dir => $opts{tmp});
          &Fxtran::Canonic::makeCanonic ($di, %opts);
          &Fxtran::Inline::inlineExternalSubroutine ($pu, $di, %opts);
        }
      
      if ($opts{'inline-contained'})
        {
          &Fxtran::Inline::inlineContainedSubroutines ($pu, find => $find, inlineDeclarations => 1, comment => $opts{'inline-comment'}, style => $opts{style});
        }
     
      @pointer = &Fxtran::Pointer::setPointersDimensions ($pu, 'no-check-pointers-dims' => $opts{'no-check-pointers-dims'})
        if ($opts{'process-pointers'});
      
      &Fxtran::Loop::removeNpromaLoops ($pu, style => $opts{style}, pointer => \@pointer);
      
      &Fxtran::ReDim::reDim ($pu, style => $opts{style}, 'redim-arguments' => $opts{'redim-arguments'});
      
      
      if ($opts{'value-attribute'})
        {
          &addValueAttribute ($pu);
        }

    }

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-singlecolumn'});
  
  unless ($opts{dummy})
    {
      &Fxtran::Call::addSuffix 
      (
        $pu, 
        suffix => $opts{'suffix-singlecolumn'}, 
        match => sub { ! $opts{style}->noComputeRoutine (@_) },
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
    }
  
  $opts{pragma}->insertRoutineSeq ($pu);

  &Fxtran::Stack::addStack 
  (
    $pu, 
    skip => sub { $opts{style}->noComputeRoutine (@_) },
    stack84 => $opts{stack84},
    style => $opts{style},
    pointer => \@pointer,
  );

  if ($opts{dummy})
    {
      &Fxtran::Interface::intfbBody ($pu->ownerDocument ());
      my ($end) = &F ('./end-subroutine-stmt', $pu);  
      my $ep = &n ('<execution-part/>');
      my $abort = &s ('CALL ABOR1_ACC ("ERROR : WRONG SETTINGS")');
      $ep->appendChild ($abort);
      $end->parentNode->insertBefore ($_, $end) for ($ep, &t ("\n"));
    }
  else
    {
      &Fxtran::Pointer::handleAssociations ($pu, pointers => \@pointer)
        if ($opts{'process-pointers'});
      
      &Fxtran::DrHook::remove ($pu) unless ($opts{drhook});
      

      unless ($opts{'merge-interfaces'})
        {
          &Fxtran::Include::removeUnusedIncludes ($pu) 
            if ($opts{style}->removeUnusedIncludes ());
        }

      $opts{style}->handleMessages ($pu, %opts);

      &Fxtran::Print::useABOR1_ACC ($pu);
      &Fxtran::Print::changeWRITEintoPRINT ($pu);
      &Fxtran::Print::changePRINT_MSGintoPRINT ($pu);
    }


}

1;
