#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

#
use strict;
use local::lib;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Getopt::Long;
use Data::Dumper;
use FileHandle;
use File::Basename;
use File::stat;
use List::MoreUtils qw (uniq);

use Fxtran;
use Decl;
use Pointer::Object;
use Pointer::SymbolTable;
use Pointer::Parallel;
use Loop;
use Call;
use Associate;
use Subroutine;
use Finder;
use Include;
use DIR;
use Bt;
use Canonic;
use Directive;
use Inline;

use Cycle49;

sub updateFile
{
  my ($F90, $code) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$F90"); $fh ? <$fh> : undef };
  
  if ((! defined ($c)) || ($c ne $code))
    {
      unlink ($F90);
      'FileHandle'->new (">$F90")->print ($code);
    }
}

sub addYDCPG_OPTS
{
  my $d = shift;
  my ($arglist) = &F ('./subroutine-stmt/dummy-arg-LT', $d);
  $arglist->appendChild (&t (','));
  $arglist->appendChild (&n ('<arg-N><N><n>YDCPG_OPTS</n></N></arg-N>'));
  &Decl::declare ($d, 'TYPE (CPG_OPTS), INTENT (IN) :: YDCPG_OPTS');
  &Decl::use ($d, 'USE CPG_OPTS_TYPE_MOD');

  my @expr = &F ('//named-E[string(N)="KIDIA" or string(N)="KFDIA"]', $d);

  for my $expr (@expr)
    {
      my $N = $expr->textContent;
      $expr->replaceNode (&e ("YDCPG_BNDS%$N"));
    }
}


my $suffix = '_parallel';

sub processSingleParallel
{
  my ($parallel, $ipar, $NAME, $t, $find, $types, $puseUtilMod, %opts) = @_;

  my $target = $parallel->getAttribute ('target');

  my @target = split (m/\//o, $target || 'OpenMP');

  my ($if_construct) = &Fxtran::parse (fragment => << "EOF");
IF (LLCOND) THEN
!
ELSEIF (LLCOND) THEN
!
ELSE
  CALL ABOR1 ('$NAME: NOT METHOD WAS FOUND')
ENDIF
EOF

  my ($if_block, $elseif_block) = &F ('./if-block', $if_construct);
  $elseif_block->unbindNode ();

  $parallel->replaceNode ($if_construct);

  my @block;
  
  my $name = $parallel->getAttribute ('name') || $ipar;
  
  # Do it once for all sections

  my %parallel;
  for my $itarget (0 .. $#target)
    {
      'Pointer::Parallel'->getWhereTargetFromTarget (my $target = $target[$itarget], my $where);
      my $class = 'Pointer::Parallel'->class ($target);
      my $onlySimpleFields = $class->onlySimpleFields ();

      my $parallel1 = $parallel->cloneNode (1);
      $parallel{$onlySimpleFields} ||= 
        &Pointer::Parallel::makeParallel ($parallel1, $t, $find, $types, "$NAME:$name", $opts{'post-parallel'}, $onlySimpleFields);

      $$puseUtilMod ||= $class->requireUtilMod ();
    }

  for my $itarget (0 .. $#target)
    {
      'Pointer::Parallel'->getWhereTargetFromTarget (my $target = $target[$itarget], my $where);
      my $class = 'Pointer::Parallel'->class ($target);
      $where ||= $class->getDefaultWhere ();
      $where = uc ($where);

      my $onlySimpleFields = $class->onlySimpleFields ();

      my $parallel1 = $parallel{$onlySimpleFields};

      unless ($parallel1)
        {
          $if_construct->unbindNode ();
          next;
        }

      $parallel1 = $parallel1->cloneNode (1);

      $parallel1 = $class->makeParallel ($parallel1, $t, %opts);
      
      my $block;
      if ($itarget == 0)
        {
          $block = $if_block;
        }
      else
        {
          $block = $elseif_block->cloneNode (1);
          $if_construct->insertAfter ($block, $block[-1]);
        }
      
      my ($cond) = &F ('./ANY-stmt/condition-E/ANY-E', $block);
      $cond->replaceNode (&e ("LPARALLELMETHOD ('$target','$NAME:$name')"));

      my ($C) = &F ('./C', $block);

      $C->replaceNode ($parallel1);

      if ($where ne 'HOST')
        {
          my @get = &F ('./prep//named-E[string(N)="GET_HOST_DATA_RDONLY" '
                              .     ' or string(N)="GET_HOST_DATA_RDWR" '
                              .     ' or string(N)="GATHER_HOST_DATA_RDONLY" '
                              .     ' or string(N)="GATHER_HOST_DATA_RDWR" '
                              .     ' ]/N/n/text()', $parallel1);
          for my $get (@get)
            {
              (my $t = $get->data) =~ s/_HOST_/_${where}_/go;
              $get->setData ($t);
            }
        }

      push @block, $block;

    }

  $if_construct->parentNode->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',0,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
  $if_construct->parentNode->insertBefore (&t ("\n"), $if_construct);


  $if_construct->parentNode->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',1,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
  $if_construct->parentNode->insertAfter (&t ("\n"), $if_construct);

}

sub processSingleRoutine
{
  my ($d, $NAME, $find, $types, %opts) = @_;

  for my $jlon  (@{ $opts{jlon} })
    {
      my @expr = &F ('.//named-E[string(N)="?"]/N/n/text()', $jlon, $d);
      for (@expr) 
        {
          $_->setData ('JLON');
        }
    }
  
  for my $unseen (&F ('.//unseen', $d))
    {
      $d->unbindNode ();
    }
  
  &Subroutine::rename ($d, sub { return $_[0] . uc ($suffix) });
  
  # Prepare the code
  
  &Associate::resolveAssociates ($d);
  
  if ($opts{'inline-contains'})
    {
      &Construct::changeIfStatementsInIfConstructs ($d);
      &Inline::inlineContainedSubroutines ($d, skipDimensionCheck => 1);
    }
  
  if ($opts{cycle} == 49)
    {
      &Cycle49::simplify ($d);
    }
  
  
  if ($opts{addYDCPG_OPTS})
    {
      &addYDCPG_OPTS ($d);
    }
  
  &Decl::forceSingleDecl ($d);
  
  &Directive::parseDirectives ($d, name => 'ACDC');
  
  # Add modules
  
  my @use = qw (FIELD_MODULE FIELD_FACTORY_MODULE FIELD_ACCESS_MODULE YOMPARALLELMETHOD STACK_MOD);
  
  if ($opts{'use-acpy'})
    {
      push @use, 'ACPY_MOD';
    }
  
  &Decl::use ($d, map { "USE $_" } @use);
  
  # Add local variables
  
  &Decl::declare 
  ($d,  
    'INTEGER(KIND=JPIM) :: JBLK',
    'TYPE(CPG_BNDS_TYPE) :: YLCPG_BNDS', 
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_FIELD_API',
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_PARALLEL',
    'REAL(KIND=JPHOOK) :: ZHOOK_HANDLE_COMPUTE',
    'TYPE(STACK) :: YLSTACK',
  );
  
  my $t = &Pointer::SymbolTable::getSymbolTable 
    ($d, skip => $opts{skip}, nproma => $opts{nproma}, 
     'types-fieldapi-dir' => $opts{'types-fieldapi-dir'},
     'types-constant-dir' => $opts{'types-constant-dir'},
     'types-fieldapi-non-blocked' => $opts{'types-fieldapi-non-blocked'});
    
  
  for my $v (qw (JLON JLEV))
    {
      &Decl::declare ($d, "INTEGER(KIND=JPIM) :: $v") unless ($t->{$v});
    }
  
  # Remove SKIP sections
  
  for (&F ('.//skip-section', $d))
    {
      $_->unbindNode ();
    }
  
  # Transform NPROMA fields into a pair of (FIELD API object, Fortran pointer)
  
  &Pointer::Parallel::fieldifyDecl 
     (
       'program-unit' => $d, 
       'symbol-table' => $t,
       'contiguous'   => $opts{contiguous},
     );

  # Process ABORT sections

  my @abort = &F ('.//abort-section', $d);

  for my $abort (@abort)
    {
      $_->unbindNode ()
        for (&F ('./*', $abort));
      $abort->appendChild ($_)
        for (&s ('CALL ABOR1 ("ERROR: WRONG SETTINGS")'), &t ("\n"));
    }
  
  # Process PARALLEL sections
  
  my @parallel = &F ('.//parallel-section', $d);
  
  my ($MESONH, $FILTER);
  
  for my $parallel (@parallel)
    {
      if (my $style = $parallel->getAttribute ('style'))
        {
          $MESONH ||= $style eq 'MESONH';
        }
      if (my $filter = $parallel->getAttribute ('filter'))
        {
          $FILTER ||= 1;
        }
    }
  
  if ($MESONH && (my ($D) = &F ('.//EN-decl[string(.)="D"]', $d)))
    {
      my $lst = $D->parentNode;
      $lst->appendChild (&t (', '));
      $lst->appendChild (&n ("<EN-decl><EN-N><N><n>DD</n></N></EN-N></EN-decl>"));
    }
  
  if ($FILTER)
    {
      &Decl::use ($d, "USE FIELD_GATHSCAT_MODULE");
      &Decl::declare ($d, 'TYPE(FIELD_GATHSCAT) :: YL_FGS');
    }
  
  # Process call to parallel routines
  
  my @call = &F ('.//call-stmt[not(ancestor::parallel-section)]' # Skip calls in parallel sections
              . '[not(string(procedure-designator)="DR_HOOK")]'  # Skip DR_HOOK calls
              . '[not(string(procedure-designator)="ABOR1")]'    # Skip ABOR1 calls
              . '[not(string(procedure-designator)="ABORT")]'    # Skip ABORT calls
              . '[not(string(procedure-designator)="GETENV")]'   # Skip ABORT calls
              . '[not(procedure-designator/named-E/R-LT)]'       # Skip objects calling methods
              . '[not(ancestor::serial-section)]', $d);          # Skip calls in serial sections
  
  my %seen;
  
  for my $call (@call)
    {
      if (&Pointer::Parallel::callParallelRoutine ($call, $t, $types))
        {
          # Add include for the parallel CALL
          my ($name) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          unless ($seen{$name->textContent}++)
            {
              my ($include) = &F ('.//include[./filename[string(.)="?" or string(.)="?"]', lc ($name) . '.intfb.h', lc ($name) . '.h', $d);
              $include or die $call->textContent;
              $include->parentNode->insertAfter (&n ('<include>#include "<filename>' . lc ($name) . '_parallel.intfb.h</filename>"</include>'), $include);
              $include->parentNode->insertAfter (&t ("\n"), $include);
            }
          $name->setData ($name->data . uc ($suffix));
        }
    }
  
  # Create/delete fields for local arrays
  
  &Pointer::Parallel::setupLocalFields ($d, $t, '', $opts{gpumemstat});
  
  &Include::removeUnusedIncludes ($d);
  
  my $useUtilMod = 0;
  
  for my $ipar (0 .. $#parallel)
    {
      &processSingleParallel ($parallel[$ipar], $ipar, $NAME, $t, $find, 
                              $types, \$useUtilMod, %opts);
    }
  
  # Declare pointers required for parallel sections
  
  my (@decl, @use_util);
  
  for my $n (sort keys (%$t))
    {
      my $s = $t->{$n};
      if ($s->{object_based})
        {
          my $nd = $s->{nd};
          $nd++ if ($s->{blocked});
          my $decl = &s ($s->{ts}->textContent . ", POINTER :: " . $n . "(" . join (',', (':') x $nd) . ")");
          push @decl, $decl;
        }
      if ($s->{object})
        {
          my ($tn) = &F ('./T-N', $s->{ts}, 1);
          if ($tn =~ m/^FIELD_\w+_ARRAY$/o)
            {
              push @use_util, &s ("USE FIELD_ARRAY_UTIL_MODULE");
            }
          else
            {
              push @use_util, &s ("USE UTIL_${tn}_MOD");
            }
        }
    }
  
  @use_util = &uniq (@use_util);
  
  
  &Decl::declare ($d, @decl);
  &Decl::use ($d, @use_util) if ($useUtilMod);
  
  # Include *_openacc.intfb.h interfaces
  
  my @called = &F ('.//call-stmt/procedure-designator', $d, 1);
  
  my @include = &F ('.//include', $d);
  
  for my $include (@include)
    {
      my ($proc) = &F ('./filename', $include, 2);
      for ($proc)
        {
          s/\.intfb\.h$//o;
          s/\.h$//o;
          $_ = uc ($_);
        }
      $proc .= '_OPENACC';
   
      if (grep { $proc eq $_ } @called)
        {
          my $include_openacc = &n ('<include>#include "<filename>' . lc ($proc) . '.intfb.h</filename>"</include>');
          $include->parentNode->insertAfter ($include_openacc, $include);
          $include->parentNode->insertAfter (&t ("\n"), $include);
        }
      
    }
  
  # Include MODI_* interfaces
  
  my @modi = &F ('.//use-stmt[starts-with(string(module-N),"MODI_")]', $d);
  
  for my $modi (@modi)
  {
    my ($proc) = &F ('./module-N', $modi, 1);
    $proc =~ s/^MODI_//o;
    if (grep { $proc eq $_ } @called)
      {
        $proc .= '_OPENACC';
        my ($use) = &s ("USE MODI_$proc");
        $modi->parentNode->insertAfter ($use, $modi);
        $modi->parentNode->insertAfter (&t ("\n"), $modi);
      }
  }
  
  
  
  if (@parallel)
    {
      # Add abor1.intfb.h
  
      unless (&F ('.//include[string(filename)="abor1.intfb.h"]', $d))
        {
          &Include::addInclude ($d, 'abor1.intfb.h');
        }
    }
  
  # include stack.h
  
  my ($implicit) = &F ('.//implicit-none-stmt', $d);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);

}


my %opts = ('types-fieldapi-dir' => 'types-fieldapi', skip => 'PGFL,PGFLT1,PGMVT1,PGPSDT2D', 
             nproma => 'YDCPG_OPTS%KLON,YDGEOMETRY%YRDIM%NPROMA', 'types-constant-dir' => 'types-constant',
             'post-parallel' => 'nullify', cycle => '49', 'jlon', 'JLON', 
             'types-fieldapi-non-blocked' => 'CPG_SL1F_TYPE,CPG_SL_MASK_TYPE');
my @opts_f = qw (help only-if-newer version stdout addYDCPG_OPTS redim-arguments stack84 use-acpy inline-contains gpumemstat contiguous);
my @opts_s = qw (skip nproma types-fieldapi-dir types-constant-dir post-parallel dir cycle jlon types-fieldapi-non-blocked files base);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

for my $opt (qw (nproma jlon types-fieldapi-non-blocked))
  {
    $opts{$opt} = [split (m/,/o, $opts{$opt})];
  }

if ($opts{help})
  {
    print
     "Usage: " . &basename ($0) . "\n" .
      join ('', map { "  --$_\n" } @opts_f) .
      join ('', map { "  --$_=...\n" } @opts_f) .
     "\n";
    exit (0);
  }

$opts{skip} = [split (m/,/o, $opts{skip} || '')];

my $F90 = shift;
(my $F90out = $F90) =~ s/.F90$/$suffix.F90/o;

unless ($opts{dir})
  {
    $opts{dir} = &dirname ($F90out);
  }

$F90out = 'File::Spec'->catpath ('', $opts{dir}, &basename ($F90out));

if ($opts{'only-if-newer'})
  {
    my $st = stat ($F90);
    my $stout = stat ($F90out);
    if ($st && $stout)
      {
        exit (0) unless ($st->mtime > $stout->mtime);
      }
  }

my $NAME = uc (&basename ($F90out, qw (.F90)));

my $find = 'Finder'->new (files => $opts{files}, base => $opts{base});

my $types = &Storable::retrieve ("$opts{'types-fieldapi-dir'}/decls.dat");

&fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 512));

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 800 -no-include -no-cpp -construct-tag -directive ACDC -canonic)]);

for my $pu (&F ('./object/file/program-unit', $d))
  {
    &processSingleRoutine ($pu, $NAME, $find, $types, %opts);
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
  }



