package Fxtran::SingleColumn;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Basename;

use strict;

use Fxtran::Common;
use Fxtran;
use Fxtran::Stack;
use Fxtran::Loop;
use Fxtran::ReDim;
use Fxtran::Subroutine;
use Fxtran::Call;
use Fxtran::Canonic;
use Fxtran::DrHook;
use Fxtran::Include;
use Fxtran::Pointer;
use Fxtran::Print;
use Fxtran::Interface;
use Fxtran::Module;


sub arraySliceToAddress
{
  my ($pu, %opts) = @_;

  my $style = $opts{style};
  
  my @nproma = $style->nproma ();
  
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my %ptr;
  
  my @decl = &F ('./T-decl-stmt[./attribute[string(attribute-N)="INTENT"]]', $dp);
  
  my $last = $decl[-1];
  
  $dp->insertAfter (&t ("\n"), $last);
  
  for my $decl (reverse (@decl))
    {
      my ($en_decl) = &F ('./EN-decl-LT/EN-decl', $decl);
      next unless (my ($as) = &F ('./array-spec', $en_decl));
  
      my @ss = &F ('./shape-spec-LT/shape-spec', $as, 1);
      next unless (grep { $ss[0] eq $_ } @nproma);
  
      my ($n) = &F ('./EN-N/N/n/text()', $en_decl, 1);
      my $stmt = &Fxtran::stmt ($en_decl);
      
      my ($t) = &F ('./_T-spec_', $stmt, 1);
  
      my $temp = &s ("fxtran_acdc_temp ($t, ${n}_PTR, " . $as->textContent . ")");
  
      $dp->insertAfter ($_, $last)
        for ($temp, &t ("\n"));
      
      $ptr{$n}++;
  
      $as->unbindNode ();
  
      my $assoc;
  
      if (&F ('./attribute[string(attribute-N)="OPTIONAL"]', $decl))
        {
          ($assoc) = &fxtran::parse (fragment => << "EOF", fopts => [qw (-construct-tag)]);
IF (PRESENT ($n)) THEN
fxtran_acdc_assoc (${n}_PTR, ${n})
ELSE
fxtran_acdc_nullptr (${n}_PTR)
ENDIF
EOF

          &Fxtran::Canonic::makeCanonicReferences ($assoc);
  
        }
      else
        {
          $assoc = &s ("fxtran_acdc_assoc (${n}_PTR, ${n})");
        }
  
      $ep->insertBefore ($_, $ep->firstChild) 
        for (&t ("\n"), $assoc, &t ("\n"));
  
    }
  
  $dp->insertAfter (&t ("\n"), $last);
  
  for my $expr (&F ('.//named-E', $ep))
    {
      my ($n) = &F ('./N/n/text()', $expr);
      next unless ($ptr{$n->textContent});
  
      my $expr = &Fxtran::expr ($n); 
      $expr = &Fxtran::expr ($expr);
  
      if ($expr && ($expr->nodeName eq 'named-E') 
         && ((&F ('./N', $expr, 1))[0] eq 'PRESENT') 
         && (&F ('./R-LT/function-R', $expr)))
        { 
          next;
        }
  
      $n->setData ($n->textContent . '_PTR');
    }

}


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
    'stack-method' => $opts{'stack-method'},
  );

}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  # Process ABORT sections

  for my $abort (&F ('.//abort-section', $pu))
    {    
      $_->unbindNode () for ($abort->childNodes ());
      $abort->appendChild ($_) 
        for (&s ('CALL ABOR1 ("ERROR: WRONG SETTINGS")'), &t ("\n"));
    }    

  my $find = $opts{find};

  my @pointer;

  unless ($opts{dummy})
    {
     
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
        suffix => $opts{'suffix-singlecolumn-called'}, 
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
    'stack-method' => $opts{'stack-method'},
  );

  if ($opts{dummy})
    {
      &Fxtran::Interface::intfbBody ($pu->ownerDocument ());
      my ($end) = &F ('./end-subroutine-stmt', $pu);  
      my $ep = &n ('<execution-part/>');
      my $abort = &s ('CALL FXTRAN_ACDC_ABORT ("ERROR : WRONG SETTINGS")');
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

  &arraySliceToAddress ($pu, %opts)
    if ($opts{'array-slice-to-address'});

}

1;
