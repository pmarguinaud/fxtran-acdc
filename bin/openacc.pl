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
use Decl;
use Dimension;
use Include;
use Inline;
use Finder;
use Pointer;
use Print;

my $SUFFIX = '_OPENACC';

sub acraneb2
{
  my $d = shift;

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
      if (my ($as) = &F ('.//T-decl-stmt//EN-decl[string(EN-N)="?"]/array-spec', $N, $d))
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
      my @en_decl = &F ('.//T-decl-stmt//EN-decl[string(EN-N)="KIIDIA" or string(EN-N)="KIFDIA"]/EN-N/N/n/text()', $d);
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

  my @intent = &F ('.//T-decl-stmt'    
                 . '[_T-spec_/intrinsic-T-spec[string(T-N)="REAL" or string(T-N)="INTEGER" or string(T-N)="LOGICAL"]]' # Only REAL/INTEGER/LOGICAL
                 . '[not(.//array-spec)]'                                                                              # Without dimensions
                 . '//attribute[string(intent-spec)="IN"]'                                                             # Only arguments
                 , $d); 

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

  if ($opts{interfaces})
    {
      my @pu = &F ('./interface-construct/program-unit', $d);
     
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

  &Dimension::attachArraySpecToEntity ($d);
  &Decl::forceSingleDecl ($d);
  
  my @KLON = ('KLON', 'YDGEOMETRY%YRDIM%NPROMA', 'YDCPG_OPTS%KLON');
  push @KLON, 'D%NIT', 'D%NIJT' if ($opts{mesonh});
  push @KLON, ('YDGEOMETRY%YRDIM%NPROMA', 'KPROMA') if ($opts{cpg_dyn});
  
  &ReDim::reDim ($d, KLON => \@KLON, 'redim-arguments' => $opts{'redim-arguments'});
  
  if ($opts{'value-attribute'})
    {
      &addValueAttribute ($d);
    }
  
  &Subroutine::addSuffix ($d, $SUFFIX);
  
  &OpenACC::routineSeq ($d);
  
  my $exec = &s ("STOP");
  $d->insertBefore ($exec, $end);
  $d->insertBefore (&t ("\n"), $end);
  
  &Stack::addStack 
  (
    $d, 
    skip => sub { my $proc = shift; grep ({ $_ eq $proc } @{ $opts{nocompute} }) },
    stack84 => $opts{stack84},
    KLON => \@KLON,
    local => 0,
  );

  $exec->unbindNode ();
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

  my (@KLON, $KIDIA, $KFDIA);

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
     
      if ($opts{jljk2jlonjlev})
        {
          &Identifier::rename ($d, JL => 'JLON', JK => 'JLEV');
        }
      
      if ($opts{jijk2jlonjlev})
        {
          &Identifier::rename ($d, JI => 'JLON', JK => 'JLEV', 'JIJ' => 'JLON');
        }
      
      if ($opts{cpg_dyn})
        {
          &Identifier::rename ($d, JROF => 'JLON');
        }
      
      &Associate::resolveAssociates ($d);
      
      &Dimension::attachArraySpecToEntity ($d);
      &Decl::forceSingleDecl ($d);
      
      if ($opts{cycle} eq '49')
        {
          &Cycle49::simplify ($d, set => $opts{'set-variables'});
        }
      
      &DIR::removeDIR ($d);
      
      @KLON = ('KLON', 'YDGEOMETRY%YRDIM%NPROMA', 'YDCPG_OPTS%KLON');
      push @KLON, 'D%NIT', 'D%NIJT' if ($opts{mesonh});
      push @KLON, ('YDGEOMETRY%YRDIM%NPROMA', 'KPROMA') if ($opts{cpg_dyn});
      
      ($KIDIA, $KFDIA) = qw (KIDIA KFDIA);
      
      if ($opts{mesonh})
        {
          ($KIDIA, $KFDIA) = ('D%NIB', 'D%NIE');
        }
      
      if ($opts{cpg_dyn})
        {
          ($KIDIA, $KFDIA) = ('KST', 'KEND')
        }
      
      @pointer = &Pointer::setPointersDimensions ($d, 'no-check-pointers-dims' => $opts{'no-check-pointers-dims'})
        if ($opts{pointers});
      
      &Loop::removeJlonLoops ($d, KLON => \@KLON, KIDIA => $KIDIA, KFDIA => $KFDIA, pointer => \@pointer, mesonh => $opts{mesonh});
      
      &ReDim::reDim ($d, KLON => \@KLON, 'redim-arguments' => $opts{'redim-arguments'});
      
      
      if ($opts{'value-attribute'})
        {
          &addValueAttribute ($d);
        }

    }
  
  &Subroutine::addSuffix ($d, $SUFFIX);
  
  unless ($opts{dummy})
    {
      &Call::addSuffix ($d, suffix => $SUFFIX, match => sub { my $proc = shift; ! grep ({ $_ eq $proc } @{ $opts{nocompute} })});
    }
  
  &OpenACC::routineSeq ($d);
  
  &Stack::addStack 
  (
    $d, 
    skip => sub { my $proc = shift; grep ({ $_ eq $proc } @{ $opts{nocompute} }) },
    stack84 => $opts{stack84},
    KLON => \@KLON,
    pointer => \@pointer,
  );

  unless ($opts{dummy})
    {
      &Pointer::handleAssociations ($d, pointers => \@pointer)
        if ($opts{pointers});
      
      &DrHook::remove ($d) unless ($opts{drhook});
      
      &Include::removeUnusedIncludes ($d) if ($opts{'remove-unused-includes'});
      
      &Print::useABOR1_ACC ($d);
      &Print::changeWRITEintoPRINT ($d);
      &Print::changePRINT_MSGintoPRINT ($d);
    }


  if ($opts{dummy})
    {
      &Fxtran::intfb_body ($d->ownerDocument ());
      my ($end) = &F ('./end-subroutine-stmt', $d);  
      my $abort = &s ('CALL ABOR1_ACC ("ERROR : WRONG SETTINGS")');
      $end->parentNode->insertBefore ($_, $end) for ($abort, &t ("\n"));
    }
}




my %opts = (cycle => 49, 'include-ext' => '.intfb.h', tmp => '.');
my @opts_f = qw (help drhook only-if-newer jljk2jlonjlev version stdout jijk2jlonjlev mesonh 
                 remove-unused-includes modi value-attribute redim-arguments stack84 
                 cpg_dyn pointers inline-contained debug interfaces dummy acraneb2 inline-comment interface);
my @opts_s = qw (dir nocompute cycle include-ext inlined no-check-pointers-dims set-variables files base tmp);

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

if ($opts{mesonh})
  {
    $opts{jijk2jlonjlev} = 1;
    $opts{'include-ext'} = '.h';
    $opts{'remove-unused-includes'} = 1;
  }


for my $opt (qw (no-check-pointers-dims inlined nocompute set-variables))
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

my $find = 'Finder'->new (files => $opts{files}, base => $opts{base});

if ($opts{acraneb2})
  {
    &acraneb2 ($d);
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
                &Fxtran::intfb ($F90out, $opts{dir}, $opts{'include-ext'});
              }
          }
      }
  }

