package Fxtran::Generate;

=head1 NAME

Fxtran::Generate

=head1 DESCRIPTION

This module contains entry points for source code transformation and generation methods.

=head1 METHODS

=cut


#
# Copyright 2025 Meteo-France
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
use Fxtran::Canonic;
use Fxtran::Util;
use Fxtran::Directive;
use Fxtran::PATH;
use Fxtran::Interface;
use Fxtran::NVTX;
use Fxtran::Finder;
use Fxtran::Style;
use Fxtran::Pragma;
use Fxtran::Cycle;
use Fxtran::Inline;

use click;

sub changeKidiaToYDCPG_OPTS 
{
  my ($d, $opts) = @_;

  my ($pu) = &F ('./object/file/program-unit', $d);

  my $style = $opts->{style};

  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  my $jlon  = $style->jlon ();

  for (&F ('.//arg-N/N/n/text()[string(.)="?"]', $kidia, $d))
    {
      $_->setData ('YDCPG_OPTS');
    }

  for (&F ('.//arg-N/N/n/text()[string(.)="?"]', $kfdia, $d))
    {
      $_->setData ('YDCPG_BNDS');
    }

  for my $expr (&F ('.//named-E[string(N)="?"]', $kidia, $d))
    {
      my $par = 0;

      my $stmt = &Fxtran::stmt ($expr);
      if (($stmt->nodeName eq 'call-stmt') && (! &F ('ancestor::parallel-section', $stmt)))
        {
          $par = 1;
        }

      $expr->replaceNode ($par ? &e ("YDCPG_OPTS") :  &e ("YDCPG_BNDS%KIDIA"));
    }

  for my $expr (&F ('.//named-E[string(N)="?"]', $kfdia, $d))
    {
      my $par = 0;

      my $stmt = &Fxtran::stmt ($expr);
      if (($stmt->nodeName eq 'call-stmt') && (! &F ('ancestor::parallel-section', $stmt)))
        {
          $par = 1;
        }

      $expr->replaceNode ($par ? &e ("YDCPG_BNDS") : &e ("YDCPG_BNDS%KFDIA"));
    }

  for my $nproma ($style->nproma ())
    {
       for (&F ('.//named-E[string(N)="?"]', $nproma, $d))
         {
           $_->replaceNode (&e ("YDCPG_OPTS%KLON"));
         }
    }

  my ($declKIDIA) = &F ('.//T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $kidia, $d);
  my ($declKFDIA) = &F ('.//T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $kfdia, $d);
  my ($declJLON)  = &F ('.//T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $jlon, $d);

  $declKIDIA->replaceNode (&s ("TYPE (CPG_OPTS_TYPE), INTENT (IN) :: YDCPG_OPTS"));
  $declKFDIA->replaceNode (&s ("TYPE (CPG_BNDS_TYPE), INTENT (IN) :: YDCPG_BNDS"));
  $declJLON->replaceNode (&s ("INTEGER (KIND=JPIM) :: JLON")) if ($declJLON);

  for (&F ('.//named-E[string(.)="?"]/N/n/text()', $jlon, $d))
    {
      $_->setData ('JLON');
    }

  $opts->{style} = 'Fxtran::Style'->new (document => $d);

  &Fxtran::Decl::use ($pu, 'USE CPG_OPTS_TYPE_MOD, ONLY : CPG_OPTS_TYPE, CPG_BNDS_TYPE');
}

my %options= do
{
  my $options = << "EOF";
  cycle=s                   -- Cycle                                                                                                        -- 49
  dir=s                     -- Dump result in this directory                                                                                -- .
  only-if-newer             -- Do not update file if unchanged content
  merge-interfaces          -- Consider that single column interfaces and regular interfaces are in the same include file
  pragma=s                  -- Pragma (OpenACC or OpenMP)                                                                                   -- OpenACC
  stack84                   -- Use separate stacks for data types of sizes 4 and 8
  stack-method              -- Use stack method instead of macros
  style=s                   -- Source code style (default: guess from file contents)
  redim-arguments           -- Transform 1D array arguments to scalars
  set-variables=s%          -- Apply variables values and simplify the code
  suffix-singlecolumn=s     -- Suffix for generated routines                                                                                -- _OPENACC
  tmp=s                     -- Temporary directory for processing                                                                           -- .
  value-attribute           -- Add VALUE attribute to scalar intrinsic arguments
  version                   -- Append fxtran-acdc version at end of generated content
  inline-contained          -- Inline contained routines
  type-bound-methods        -- Generate & use type bound methods
  types-constant-dir=s      -- Directory with constant type information                                                                     --  types-constant
  types-fieldapi-dir=s      -- Directory with Field API type information                                                                    --  types-fieldapi
  suffix-pointerparallel=s  -- Suffix for parallel routines                                                                                 --  _PARALLEL
  suffix-singleblock=s      -- Suffix for single block routines                                                                             --  _SINGLEBLOCK
  suffix-manyblocks=s       -- Suffix for many blocks routines                                                                              --  _MANYBLOCKS
  ydcpg_opts                -- Change KIDIA, KFDIA -> YDCPG_OPTS, YDCPG_BNDS
  checker                   -- Sanity checks, produce a report
  suffix-semiimplicit=s     -- Suffix for semi-implicit  routines                                                                           --  _SINGLEBLOCK
  base                      -- Base directory for file search                                                                               -- .
  array-slice-to-address    -- Pass addresses of first array element instead of array slices
  use-stack-manyblocks      -- Use stack allocation for manyblocks routines
  method-prefix=s           -- Prefix for method names                                                         -- ACDC_
  use-bit-repro-intrinsics  -- Use bit reproducible intrinsics
  suffix-bitrepro=s         -- Suffix for bit-repro routines                                                   -- _BITREPRO
EOF

  my @options;

  for (split (m/\n/o, $options))
    {
      my ($opt) = (m/^\s*([\w-]+)/o);
      push @options, $opt, "$_\n";
    }

  @options
};

sub routineToRoutineHead
{
  my ($F90, $method, $opts, @fopts) = @_;

  $opts->{dir} = 'File::Spec'->rel2abs ($opts->{dir});
  
  if ($opts->{dir} ne 'File::Spec'->rel2abs (&dirname ($F90)))
    {
      &copy ($F90, join ('/', $opts->{dir}, &basename ($F90)));
    }

  my $F90out = $F90;

  if ($opts->{"suffix-$method"})
    {
      $F90out =~ s{.F90$}{lc ($opts->{"suffix-$method"}) . '.F90'}eo;
    }
  
  $F90out = 'File::Spec'->catpath ('', $opts->{dir}, &basename ($F90out));
  
  if ($opts->{'only-if-newer'})
    {
      my $st = stat ($F90);
      my $stout = stat ($F90out);
      if ($st && $stout)
        {
          return unless ($st->mtime > $stout->mtime);
        }
    }
  
  $opts->{find} = 'Fxtran::Finder'->new (files => $opts->{files}, base => $opts->{base}, I => $opts->{I});
  
  &fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 1024));
  &fxtran::setOptions (qw (Statement -line-length 1024));
  
  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 5000 -no-include -no-cpp -construct-tag -canonic), @fopts], dir => $opts->{tmp});

  &Fxtran::Canonic::makeCanonic ($d, %$opts);

  die ("Could not figure out style for file $F90")
    unless ($opts->{style} = 'Fxtran::Style'->new (%$opts, document => $d));

  $opts->{pragma} = 'Fxtran::Pragma'->new (%$opts);

  if ($opts->{checker})
    {
      &Fxtran::Util::loadModule ('Fxtran::Generate::Checker');
      'Fxtran::Generate::Checker'->$method ($d, %$opts)
    }

  if ((scalar (@{ $opts->{inlined} || [] }) || $opts->{'inline-contained'}) && (! $opts->{dummy}))
    {
      &Fxtran::Include::loadContainedIncludes ($d, %$opts)
        if ($opts->{'inline-contained'});

      my $find = $opts->{find};

      for my $pu (&F ('.//program-unit', $d))
        {
          my $stmt = $pu->firstChild;
          next unless ($stmt->nodeName eq 'subroutine-stmt');

          for my $in (@{ $opts->{inlined} || [] })
            {   
              my $f90in = $find->resolve (file => $in);
              my $di = &Fxtran::parse (location => $f90in, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)], dir => $opts->{tmp});
              &Fxtran::Canonic::makeCanonic ($di, %$opts);
              &Fxtran::Inline::inlineExternalSubroutine ($pu, $di, %$opts);
            }   

          if ($opts->{'inline-contained'})
            {
              &Fxtran::Inline::inlineContainedSubroutines ($pu, skipDimensionCheck => 1, inlineDeclarations => 1, 
                                                           comment => $opts->{'inline-comment'}, find => $find,
                                                           style => $opts->{style});
            }
        }
    }

  if ($opts->{'use-bit-repro-intrinsics'})
    {
      for my $pu (&F ('.//program-unit', $d))
        {
          my $stmt = $pu->firstChild;
          next unless ($stmt->nodeName eq 'subroutine-stmt');
          &Fxtran::Intrinsic::makeBitReproducible ($pu, %$opts);
        }
    }
  
  return ($d, $F90out);
}

sub routineToRoutineTail
{
  my ($F90out, $d, $opts) = @_;

  &Fxtran::Util::addVersion ($d)
    if ($opts->{version});
  
  &Fxtran::Util::updateFile ($F90out, &Fxtran::Canonic::indent ($d));
}

&click (<< "EOF");
@options{qw (cycle dir only-if-newer merge-interfaces pragma stack84 stack-method style redim-arguments set-variables 
             suffix-semiimplicit tmp value-attribute version inline-contained checker)}
  keep-drhook               -- Keep DrHook
  dummy                     -- Generate a dummy routine (strip all executable code)
  inlined=s@                -- List of routines to inline
  inline-comment            -- Add a comment when inlining a routine
  create-interface          -- Generate an interface file
  openmptoparallel          -- Transform OpenMP parallel sections into ACDC parallel sections
  max-statements-per-parallel=s   -- Maximum number of statements per parallel section
  parallel-iterator-list=s@       -- List of iterators for generating parallel sections (add to JLON, JLEV)
EOF
sub semiimplicit
{
  my ($opts, @args) = @_;

=head2 semiimplicit

This is the method for transforming top-level semi-implicit routines such as F<spcsi.F90> (hydrostatic)
and F<spnhsi.F90> (non-hydrostatic).

The principle is to transform all zones with parallelism (C<DO JSP>) into OpenACC or OpenMP kernels. Vertical operator
routines are supposed to be transformed with the singleblock method and called from F<spcsi.F90> or F<spnhsi.F90>.

Horizontal operators are supposed to be flagged with C<!$ACDC HORIZONTAL> directives; the accelerated code for these
routines is supposed to be enabled by passing an extra argument C<LDACC=.TRUE.> to the original horizontal routine.

See L<Fxtran::SemiImplicit> for more details.

=cut

  &Fxtran::Util::loadModule ('Fxtran::SemiImplicit');

  my ($F90) = @args;

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'semiimplicit', $opts, qw (-directive ACDC), $opts->{openmptoparallel} ? ('-openmp') : ());

  if ($opts->{openmptoparallel})
    {
      &Fxtran::Directive::openmpToACDC ($d, %$opts);
    }
  
  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

  $opts->{style}->preProcessForOpenACC ($d, %$opts);
  
  my @pu = &F ('./object/file/program-unit', $d);
  
  for my $pu (@pu)
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;
      if ($kind eq 'subroutine')
        {
          &Fxtran::SemiImplicit::processSingleRoutine ($pu, %$opts);
        }
      else
        {
          die;
        }
    }
  
  &routineToRoutineTail ($F90out, $d, $opts);

  if ($opts->{'create-interface'})
    {
      $opts->{style}->generateInterface ($F90out, %$opts);
    }
}

&click (<< "EOF");
@options{qw (cycle dir only-if-newer merge-interfaces pragma stack84 stack-method style redim-arguments set-variables 
             suffix-singlecolumn tmp value-attribute version inline-contained checker array-slice-to-address use-bit-repro-intrinsics)}
  keep-drhook                  -- Keep DrHook
  dummy                        -- Generate a dummy routine (strip all executable code)
  inlined=s@                   -- List of routines to inline
  inline-comment               -- Add a comment when inlining a routine
  create-interface             -- Generate an interface file
  process-interfaces           -- Transform interfaces into single column interfaces (used for MODI MESONH files)
  no-check-pointers-dims=s@    -- List of pointer variables that should not be checked for their dimensions
  process-pointers             -- Process pointers (change them to CRAY pointers)
  suffix-singlecolumn-called=s -- Suffix for singlecolumn routines called by routine being processed
EOF
sub singlecolumn
{
  my ($opts, @args) = @_;

=head2 singlecolumn

This is the method used to transform a full vector routine (ie processing a full C<NPROMA> block) 
into its single-column version, ready for accelerators:

This involves the following steps:

=over 4

=item

Remove all loops on the C<NPROMA> (aka C<KLON> dimension).

=item

Set the iterator C<JLON> (resp. C<JROF>) to C<KIDIA> (resp. C<KST>).

=item 

Allocate temporary arrays in a pre-allocated a stack (C<YDSTACK>). These arrays are shared
by all threads belonging to the same warp.

=item

Insert an OpenACC C<!$acc routine seq>) directive.

=back

See L<Fxtran::SingleColumn> for more details.

=cut

  $opts->{'suffix-singlecolumn-called'} ||= $opts->{'suffix-singlecolumn'};

  &Fxtran::Util::loadModule ('Fxtran::SingleColumn');

  my ($F90) = @args;

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'singlecolumn', $opts, qw (-directive ACDC));

  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

  $opts->{style}->preProcessForOpenACC ($d, %$opts);
  
  my @pu = &F ('./object/file/program-unit', $d);
  
  my $singleRoutine = scalar (@pu) == 1;
  
  for my $pu (@pu)
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;
      if ($kind eq 'module')
        {
          $singleRoutine = 0;
          &Fxtran::SingleColumn::processSingleModule ($pu, %$opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &Fxtran::SingleColumn::processSingleRoutine ($pu, %$opts);
        }
      else
        {
          die;
        }
    }
  
  &routineToRoutineTail ($F90out, $d, $opts);

  if ($opts->{'create-interface'} && $singleRoutine)
    {
      $opts->{style}->generateInterface ($F90out, %$opts);
    }
}


&click (<< "EOF");
@options{qw (cycle dir tmp only-if-newer merge-interfaces pragma stack84 stack-method style redim-arguments ydcpg_opts checker suffix-manyblocks
             suffix-singlecolumn suffix-pointerparallel version type-bound-methods types-constant-dir types-fieldapi-dir method-prefix use-stack-manyblocks)}
  base                            -- Base directory for file lookup
  contiguous-pointers             -- Add CONTIGUOUS attribute to pointer accessors
  files=s@                        -- List of files to be looked at for inlining
  gpumemstat                      -- Add calls to GPUMEMSTAT
  inlined=s@                      -- List of routines to inline
  inline-contained                -- Inline CONTAINed routines
  post-parallel=s@                -- Generate code after parallel section                                                                  --  nullify
  skip-arrays=s@                  -- Arrays not to be processed                                                                            --  PGFL,PGFLT1,PGMVT1,PGPSDT2D
  types-fieldapi-non-blocked=s@   -- Non-blocked data types (without NPROMA)                                                               --  CPG_SL1F_TYPE,CPG_SL_MASK_TYPE
  use-acpy                        -- Avoid pointer aliasing using ACPY
  use-bcpy                        -- Avoid pointer aliasing using BCPY
  parallelmethod-section          -- Embed parallelmethod information in binary
  create-interface                -- Generate an interface file
EOF
sub pointerparallel
{
  my ($opts, @args) = @_;

=head2 pointerparallel

This method transforms a vector routine (processing a single C<NPROMA> block) into a parallel routines, that is,
a routine containing many OpenMP/OpenACC kernels.

=over 4

=item

C<!$ACDC PARALLEL> sections are searched.

=item

Each section, depending on the user-provided options, may be transformed into the one or many of the following 
kernels: OpenMP, OpenMPSingleColumn, OpenACCSingleColumn

The selection of the kernel variant is chosen at run time.

=item

Some instrumentation may be added: measurement of GPU memory, synchronisation of data on the host 
after each kernel.

=back

See L<Fxtran::Pointer::Parallel> for more details.

=cut

  &Fxtran::Util::loadModule ('Fxtran::Pointer::Parallel');
  &Fxtran::Util::loadModule ('Fxtran::IO::Link');

  my ($F90) = @args;

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'pointerparallel', $opts, qw (-directive ACDC));

  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');
  
  if ($opts->{ydcpg_opts})
    {
      &changeKidiaToYDCPG_OPTS ($d, $opts);
    }
  
  my @pu = &F ('./object/file/program-unit', $d);
  
  for my $pu (@pu)
    {
      $opts->{style}->preProcessForOpenACC ($pu, %$opts);
    }
  
  my $NAME = uc (&basename ($F90out, qw (.F90)));
  
  my $linkTypes = &Fxtran::IO::Link::link ('types-fieldapi-dir' => $opts->{'types-fieldapi-dir'});
  my $types = $linkTypes->{decls};
  
  for my $pu (@pu)
    {
      &Fxtran::Pointer::Parallel::processSingleRoutine ($pu, $NAME, $types, %$opts);
    }

  &routineToRoutineTail ($F90out, $d, $opts);
  
  if ($opts->{'parallelmethod-section'})
    {
      &Fxtran::Util::loadModule ('Fxtran::Generate::ParallelMethod');
      &Fxtran::Generate::ParallelMethod::generateCCode ($d, $opts);
    }

  if ($opts->{'create-interface'})
    {
      $opts->{style}->generateInterface ($F90out, %$opts);
    }
}

&click (<< "EOF");
@options{qw (cycle dir base tmp only-if-newer merge-interfaces pragma stack84 stack-method style 
             suffix-singlecolumn suffix-singleblock version checker)}
  drhooktonvtx                    -- Change DrHook calls into NVTX calls
  inlined=s@                      -- List of routines to inline
  openmptoparallel                -- Transform OpenMP parallel sections into ACDC parallel sections
  max-statements-per-parallel=s   -- Maximum number of statements per parallel section
  parallel-iterator-list=s@       -- List of iterators for generating parallel sections (add to JLON, JLEV)
EOF
sub singleblock
{
  my ($opts, @args) = @_;

=head2 singleblock

This transforms a vector routine (processing a single C<NPROMA> block) into a routine where each
loop on the C<NPROMA> dimension is transformed into an OpenACC kernel.

Arguments (C<NPROMA> arrays and structure holding constant data such as C<YDMODEL>) 
are supposed to be present on the device when the generated routine is called. 

See L<Fxtran::SingleBlock> for more details.

=cut

  &Fxtran::Util::loadModule ('Fxtran::SingleBlock');

  my ($F90) = @args;

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'singleblock', $opts, qw (-directive ACDC), $opts->{openmptoparallel} ? ('-openmp') : ());

  if ($opts->{openmptoparallel})
    {
      &Fxtran::Directive::openmpToACDC ($d, %$opts);
    }
  
  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');
  
  my @pu = &F ('./object/file/program-unit', $d);

  my $NAME = uc (&basename ($F90out, qw (.F90)));
  
  for my $pu (@pu)
    {
      &Fxtran::SingleBlock::processSingleRoutine ($pu, %$opts);
    }
  
  @pu = &F ('./object/file/program-unit', $d);

  if ($opts->{'drhooktonvtx'})
    {
      for my $pu (@pu)
        {
          &Fxtran::NVTX::drHookToNVTX ($pu);
        }
    }
  
  &routineToRoutineTail ($F90out, $d, $opts);
}

&click (<< "EOF");
@options{qw (cycle dir base tmp only-if-newer merge-interfaces pragma stack84 stack-method style inline-contained
             suffix-singlecolumn suffix-manyblocks version checker array-slice-to-address use-stack-manyblocks)}
  drhooktonvtx                    -- Change DrHook calls into NVTX calls
  inlined=s@                      -- List of routines to inline
  create-interface                -- Generate an interface file
  fuse-outer-dimension-names=s%   -- Fuse outer dimensions
  not-present-types=s@            -- List of derived types not present on the device
  max-statements-per-parallel=s   -- Maximum number of statements per parallel section
  parallel-iterator-list=s@       -- List of iterators for generating parallel sections (add to JLON, JLEV)
EOF
sub manyblocks
{
  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::ManyBlocks');

  my ($F90) = @args;

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'manyblocks', $opts, qw (-directive ACDC));

  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');
  
  my @pu = &F ('./object/file/program-unit', $d);

  my $NAME = uc (&basename ($F90out, qw (.F90)));
  
  for my $pu (@pu)
    {
      &Fxtran::ManyBlocks::processSingleRoutine ($pu, %$opts);
    }
  
  @pu = &F ('./object/file/program-unit', $d);

  if ($opts->{'drhooktonvtx'})
    {
      for my $pu (@pu)
        {
          &Fxtran::NVTX::drHookToNVTX ($pu);
        }
    }
  
  &routineToRoutineTail ($F90out, $d, $opts);

  if ($opts->{'create-interface'})
    {
      $opts->{style}->generateInterface ($F90out, %$opts);
    }
}

&click (<< "EOF");
@options{qw (dir pragma tmp type-bound-methods types-constant-dir types-fieldapi-dir checker method-prefix)}
  field-api                       -- Dump Field API information
  field-api-class=s               -- Field API structure category
  methods-list=s@                 -- List of methods (copy, crc64, host, legacy, load, save, size, wipe
  module-map=s%                   -- Type/module mapping for methods
  no-allocate=s@                  -- Structures that should not be allocated/deallocated
  only-components=s               -- Process only these derived type members
  only-types=s                    -- Process only these derived types
  out                             -- Output file name
  skip-components=s               -- Skip these derived type members
  skip-types=s                    -- Skip these derived types
  sorted                          -- Sort files (with number prefix) in compilation order
  numbered-submodules             -- Do not generate submodules with full names, use numbers instead
  split-util                      -- Split util module into several modules (one per method)
EOF
sub methods
{
  use List::MoreUtils qw (uniq);

  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::IO');
  &Fxtran::Util::loadModule ('Fxtran::FieldAPI::Register');
  &Fxtran::Util::loadModule ('Fxtran::Pragma');

  my %methods = map { ($_, 1) } @{ $opts->{'methods-list'} };
  for my $method (qw (copy crc64 host legacy load save size wipe))
    {
      $opts->{$method} = 1 if ($methods{$method});
    }

  my ($F90) = @args;  

  if ($opts->{'type-bound-methods'})
    {
      if (&dirname ($F90) eq $opts->{dir})
        {
          die ("Dumping code in `$opts->{dir}` would overwrite `$F90'");
        }
    }
  elsif ($opts->{dir} ne 'File::Spec'->rel2abs (&dirname ($F90)))
    {
      &copy ($F90, join ('/', $opts->{dir}, &basename ($F90)));
    }

  ( -d $opts->{dir}) or &mkpath ($opts->{dir});
  ( -d $opts->{'types-fieldapi-dir'}) or &mkpath ($opts->{'types-fieldapi-dir'});
  ( -d $opts->{'types-constant-dir'}) or &mkpath ($opts->{'types-constant-dir'});
  
  $opts->{pragma} = 'Fxtran::Pragma'->new (%$opts);
  
  for my $k (qw (RM RB IM LM RD))
    {
      for my $i (1 .. 5)
        {
          push @{ $opts->{'no-allocate'} }, "FIELD_${i}${k}";
        }
    }
  
  @{ $opts->{'no-allocate'} } = &uniq (sort (@{ $opts->{'no-allocate'} }));
  
  for my $k (qw (RM RB IM LM RD))
    {
      for my $i (1 .. 5)
        {
          for my $e ("", "_PTR", "_VIEW")
            {
              $opts->{'module-map'}{"UTIL_FIELD_${i}${k}${e}_MOD"}       = "FIELD_UTIL_MODULE";
              $opts->{'module-map'}{"UTIL_FIELD_${i}${k}${e}_ARRAY_MOD"} = "FIELD_ARRAY_UTIL_MODULE";
            }
        }
    }
  

  my $parseListOrCodeRef = sub
  {
    use FindBin qw ($Bin);

    my ($opts, $kw) = @_;
  
    if (-f "$Bin/../lib/$opts->{$kw}.pm")
      {
        my $class = 'Fxtran::IO::' . $opts->{$kw};
        eval "use $class;";
        my $c = $@;
        $c && die ($c);
        $opts->{$kw} = sub { $class->skip (@_) };
      }
    elsif ($opts->{$kw} =~ m/^sub /o)
      {
        $opts->{$kw} = eval ($opts->{$kw});
        my $c = $@;
        die $c if ($c);
      }
    elsif ($kw =~ m/-components$/o)
      {
        my @comp = split (m/,/o, $opts->{$kw});
        if ($kw =~ m/^skip-/o)
          {
            $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
          }
        else
          {
            $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
          }
      }
    elsif ($kw =~ m/-types$/o)
      {
        my @type = split (m/,/o, $opts->{$kw});
        if ($kw =~ m/^skip-/o)
          {
            $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
          }
        else
          {
            $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
          }
      }
  };
  
  my $parseSkipOnly = sub
  {
    my ($opts, $skip, $only) = @_;
    if ($opts->{$skip})
      {
        $parseListOrCodeRef->($opts, $skip);
      }
    elsif ($opts->{$only})
      {
        $parseListOrCodeRef->($opts, $only);
        $opts->{$skip} = sub { ! $opts->{$only}->(@_) };
      }
    else
      {
        $opts->{$skip} = sub { 0 };
      }
  };
  
  $parseSkipOnly->($opts, 'skip-components', 'only-components');
  $parseSkipOnly->($opts, 'skip-types', 'only-types');
  
  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 5000 -construct-tag -no-include)], dir => $opts->{tmp});
  
  if ($opts->{load} || $opts->{save} || $opts->{size} || $opts->{copy} || $opts->{host} || $opts->{crc64} || $opts->{legacy})
    {
      &Fxtran::IO::processTypes ($d, $opts);
    }
  
  if ($opts->{'field-api'})
    {
      &Fxtran::FieldAPI::Register::registerFieldAPI ($d, $opts);
    }


}

&click (<< "EOF");
@options{qw (dir pragma tmp merge-interfaces suffix-singlecolumn suffix-singleblock suffix-pointerparallel suffix-manyblocks suffix-bitrepro
             use-stack-manyblocks ydcpg_opts cycle suffix-semiimplicit)}
EOF
sub interface
{
  my ($opts, @args) = @_;

  my ($F90) = @args;

  my $ext = '.intfb.h';

  my @D = @{ $opts->{D} };
  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 5000 -construct-tag -no-include), @D], dir => $opts->{tmp});

  my @text = split (m/\n/o, $d->textContent);
  
  &Fxtran::Interface::intfbBody ($d);

  'Fxtran::Cycle'->simplify ($d, %$opts);

  my %intfb;
  
  # Strip empty lines
  
  $intfb{regular} = $d->textContent ();
  $intfb{regular} =~ s/^\s*\n$//goms;

  &Fxtran::Util::loadModule ('Fxtran::Generate::Interface');

  my @method = qw (singlecolumn singleblock pointerparallel manyblocks bitrepro semiimplicit);

  for my $method (@method)
    {
      &Fxtran::Generate::Interface::interface ($d, \@text, $opts, \%intfb, $method);
    }

  my $sub = &basename ($F90, qw (.F90));
  
  &Fxtran::Util::updateFile 
  (
    "$opts->{dir}/$sub$ext", 
    join ("\n", 'INTERFACE', map ({ $intfb{$_} } ('regular', @method)), 'END INTERFACE', '')
  );

}

&click (<< "EOF");
@options{qw (use-bit-repro-intrinsics tmp cycle dir merge-interfaces inline-contained suffix-bitrepro)}
  use-bit-repro-parens      -- Make sure additions are executed in the right order
EOF
sub bitrepro
{
  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::BitRepro');

  my ($F90) = @args;

  $opts->{'use-bit-repro-intrinsics'} = 0; # Avoid doing the processing twice

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'bitrepro', $opts);

  &Fxtran::BitRepro::makeBitReproducible ($d, %$opts);

  &routineToRoutineTail ($F90out, $d, $opts);
}

&click (<< "EOF");
@options{qw (tmp cycle dir suffix-pointerparallel types-constant-dir types-fieldapi-dir method-prefix)}
  switch=s                  -- Set this variable to true if the parallel mode is enabled
  parallelmethod-section    -- Embed parallelmethod information in binary
EOF
sub toplevel
{
  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::Pointer::Parallel');
  &Fxtran::Util::loadModule ('Fxtran::IO::Link');

  my ($F90) = @args;

  if (&dirname ($F90) eq $opts->{dir})
    {
      die ("Dumping code in `$opts->{dir}` would overwrite `$F90'");
    }

  my ($d, $F90out) = &routineToRoutineHead ($F90, 'toplevel', $opts, qw (-openmp -directive ACDC));

  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

  my $linkTypes = &Fxtran::IO::Link::link ('types-fieldapi-dir' => $opts->{'types-fieldapi-dir'});

  &Fxtran::Util::loadModule ('Fxtran::TopLevel');

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;
      if ($kind eq 'subroutine')
        {
          &Fxtran::TopLevel::processSingleRoutine ($pu, %$opts, 'types-field-api' => $linkTypes);
        }
      else
        {
          die;
        }
    }

  &routineToRoutineTail ($F90out, $d, $opts);

  if ($opts->{'parallelmethod-section'})
    {
      &Fxtran::Util::loadModule ('Fxtran::Generate::ParallelMethod');
      &Fxtran::Generate::ParallelMethod::generateCCode ($d, $opts);
    }

}

=head1 SEE ALSO

L<fxtran-f90>, L<fxtran-gen>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=cut

1;
