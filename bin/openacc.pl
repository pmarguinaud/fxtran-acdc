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

sub acraneb2
{
  my $d = shift;
  my %args = @_;

  my ($file) = &F ('./object/file/@name', $d, 2);

  $file = &basename ($file);

# 'FileHandle'->new (">1.$file")->print (&Canonic::indent ($d));

  # Change KJN -> KLON

  my @kjn = &F ('.//named-E[string(N)="KJN"]', $d);

  for my $kjn (@kjn)
    {
      $kjn->replaceNode (&e ('KLON'));
    }


  # Remove loops on JN

  my @do = &F ('.//do-construct[./do-stmt[string(do-V)="JN"]]', $d);

  for my $do (@do)
    {
      my @c = $do->childNodes ();
      my @d = (splice (@c, 0, 2), splice (@c, -2, 2));

      for (@d)
        {
          $_->unbindNode ();
        }

     my $p = $do->parentNode ();

      for (@c)
        {
          $p->insertBefore ($_, $do);
        }
      
      $do->unbindNode ();
    }

  # Remove IIDIA/IFDIA initialization

  my @assign = &F ('.//a-stmt[./E-1/named-E[string(N)="?" or string(N)="?"]]', 'IIDIA', 'IFDIA', $d);

  for (@assign)
    {
      $_->unbindNode ();
    }


  # Use KIDIA/KFDIA instead of IIDIA/IFDIA (expressions)

  for my $T (qw (I F))
    {
      for my $e (&F ('.//named-E[string(N)="?"]', "I${T}DIA", $d))
        {
          $e->replaceNode (&e ("K${T}DIA"));
        }
    }

  # Use KIDIA/KFDIA instead of KIIDIA/KIFDIA (expressions)

  for my $T (qw (I F))
    {
      for my $e (&F ('.//named-E[string(N)="?"]', "KI${T}DIA", $d))
        {
          $e->replaceNode (&e ("K${T}DIA"));
        }
    }

  # Make KIIDIA/KIFDIA arguments scalars

  for my $N (qw (KIIDIA KIFDIA))
    {
      if (my ($as) = &F ('./T-decl-stmt//EN-decl[string(EN-N)="?"]/array-spec', $N, $d))
        {
          $as->unbindNode ();
        }
    }

  # Change KIIDIA/KIFDIA into KIDIA/KFDIA (arguments), unless KIDIA/KFDIA are already arguments of the routine
  
  my @arg = &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $d);

  unless (grep { $_->textContent eq 'KIDIA' } @arg)
    {
      for my $arg (@arg)
        {
          if ($arg->textContent =~ m/^KI([IF])DIA$/o)
            {
              my $T = $1;
              $arg->setData ("K${T}DIA");
            }
        }
      my @en_decl = &F ('./T-decl-stmt//EN-decl[string(EN-N)="KIIDIA" or string(EN-N)="KIFDIA"]/EN-N/N/n/text()', $d);
      for my $en_decl (@en_decl)
        {
          (my $n = $en_decl->textContent) =~ s/^KI/K/o;
          $en_decl->setData ($n);
        }
    }

# 'FileHandle'->new (">2.$file")->print (&Canonic::indent ($d));

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
    local => 0,
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
          &Inline::inlineExternalSubroutine ($d, $di);
        }
      
      if ($opts{'inline-contained'})
        {
          &Inline::inlineContainedSubroutines ($d, find => $find, inlineDeclarations => 1, comment => $opts{'inline-comment'});
        }
     
      if ($opts{cpg_dyn})
        {
          &Identifier::rename ($d, JROF => 'JLON');
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
      
      &Print::useABOR1_ACC ($d);
      &Print::changeWRITEintoPRINT ($d);
      &Print::changePRINT_MSGintoPRINT ($d);
    }


}


my %opts = (cycle => 49, tmp => '.', style => 'MFPHYS');
my @opts_f = qw (help drhook only-if-newer version stdout 
                 modi value-attribute redim-arguments stack84 
                 pointers inline-contained debug interfaces dummy acraneb2 inline-comment interface);
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


$opts{style} = 'Style'->new (%opts);

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

'FileHandle'->new (">toto.F90.xml")->print ($d->toString);

my $find = 'Finder'->new (files => $opts{files}, base => $opts{base});

if ($opts{acraneb2})
  {
    &acraneb2 ($d, %opts);
  }

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

