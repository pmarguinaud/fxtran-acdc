package Gen;

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Basename;

use strict;

use SingleColumn;
use Fxtran;
use Canonic;
use Util;

use click;

&click
(
  'cycle=s                  -- Cycle                                                                                                        -- 49',
  'dir=s                    -- Dump result in this directory                                                                                -- .',
  'drhook                   -- Keep DrHook                                                                                                  ',
  'dummy                    -- Generate a dummy routine (strip all executable code)                                                         ',
  'inlined=s                -- List of routines to inline                                                                                   --',
  'inline-comment           -- Add a comment when inlining a routine                                                                        ',
  'inline-contained         -- Inline contained routines                                                                                    ',
  'interface                -- Generate an interface file                                                                                   ',
  'interfaces               -- Transform interfaces into single column interfaces (used for MODI MESONH files)                              ',
  'merge-interfaces         -- Consider that single column interfaces and regular interfaces are in the same include file                   ',
  'no-check-pointers-dims=s -- List of pointer variables that should not be checked for their dimensions                                    --',
  'only-if-newer            -- Do not update file if unchanged content                                                                      ',
  'pointers                 -- Process pointers (change them to CRAY pointers                                                               ',
  'pragma=s                 -- Pragma (OpenACC or OpenMP)                                                                                   -- OpenACC',
  'redim-arguments          -- Transform 1D array arguments to scalars                                                                      ',
  'set-variables=s          -- Apply variables values and simplify the code                                                                 --',
  'stack84                  -- Use separate stacks for data types of sizes 4 and 8                                                          ',
  'style=s                  -- Source code style (default: guess from file contents)                                                        ',
  'stdout                   -- Dump generated code to stdout                                                                                ',
  'suffix=s                 -- Suffix for generated routines                                                                                -- _OPENACC',
  'tmp=s                    -- Temporary directory for processing                                                                           -- .',
  'value-attribute          -- Add VALUE attribute to scalar intrinsic arguments                                                            ',
  'version                  -- Append fxtran-acdc version at end of generated content                                                       ',
);
sub singlecolumn
{
  my ($opts, @args) = @_;

  for my $opt (qw (no-check-pointers-dims inlined set-variables))
    {
      $opts->{$opt} = [$opts->{$opt} ? split (m/,/o, $opts->{$opt}) : ()];
    }
  
  my ($F90) = @args;

  $opts->{dir} ||= &dirname ($F90);
  
  my $suffix = lc ($opts->{suffix});
  (my $F90out = $F90) =~ s/\.F90/$suffix.F90/;
  $F90out = $opts->{dir} . '/' . &basename ($F90out);
  
  
  if ($opts->{'only-if-newer'})
    {
      my $st = stat ($F90);
      my $stout = stat ($F90out);
      if ($st && $stout)
        {
          exit (0) unless ($st->mtime > $stout->mtime);
        }
    }

  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-canonic -construct-tag -no-include -no-cpp -line-length 5000)], dir => $opts->{tmp});
  
  &Canonic::makeCanonic ($d);
  
  $opts->{style} = 'Style'->new (%$opts, document => $d);
  
  $opts->{pragma} = 'Pragma'->new (%$opts);
  
  my $find = 'Finder'->new (files => $opts->{files}, base => $opts->{base});
  
  $opts->{style}->preProcessForOpenACC ($d, %$opts, find => $find);
  
  my @pu = &F ('./object/file/program-unit', $d);
  
  my $singleRoutine = scalar (@pu) == 1;
  
  for my $pu (@pu)
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;
      if ($kind eq 'module')
        {
          $singleRoutine = 0;
          &SingleColumn::processSingleModule ($pu, $find, %$opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &SingleColumn::processSingleRoutine ($pu, $find, %$opts);
        }
      else
        {
          die;
        }
    }
  
  
  &Util::addVersion ($d)
    if ($opts->{version});
  
  if ($opts->{stdout})
    {
      print &Canonic::indent ($d);
    }
  else
    {
      &mkpath (&dirname ($F90out));
      'FileHandle'->new (">$F90out.xml")->print ($d->toString);
      &Util::updateFile ($F90out, &Canonic::indent ($d));
  
  
      if ($opts->{interface} && $singleRoutine)
        {
          $opts->{style}->generateInterface ($F90out, %$opts);
        }
    }
}


&click
(
  'base                            -- Base directory for file lookup                                                                        ',
  'contiguous                      -- Add CONTIGUOUS attribute to pointer accessors                                                         ',
  'cycle=s                         -- Cycle                                                                                                 --  49',
  'dir=s                           -- Dump result in this directory                                                                         ',
  'files=s                         -- List of files to be looked at for inlining                                                            ',
  'gpumemstat                      -- Add calls to GPUMEMSTAT                                                                               ',
  'inline-contains                 -- Inline CONTAINed routines                                                                             ',
  'merge-interfaces                -- Consider that single column interfaces and regular interfaces are in the same include file            ',
  'only-if-newer                   -- Do not update file if unchanged content                                                               ',
  'post-parallel=s                 -- Generate code after parallel section                                                                  --  nullify',
  'pragma=s                        -- Pragma (OpenACC or OpenMP)                                                                            --  OpenACC',
  'redim-arguments                 -- Transform 1D array arguments to scalars                                                               ',
  'skip=s                          -- Arrays not to be processed                                                                            --  PGFL,PGFLT1,PGMVT1,PGPSDT2D',
  'stack84                         -- Use separate stacks for data types of sizes 4 and 8                                                   ',
  'stdout                          -- Dump generated code to stdout                                                                         ',
  'style=s                         -- Source code style (default: guess from file contents)                                                 ',
  'suffixParallel                  -- Suffix for parallel routines                                                                          --  _PARALLEL',
  'suffixSingleColumn              -- Suffix for single column routines                                                                     --  _OPENACC',
  'type-bound-methods              -- Generate & use type bound methods (LOAD, COPY, etc.)                                                  ',
  'types-constant-dir=s            -- Directory with constant type information                                                              --  types-constant',
  'types-fieldapi-dir=s            -- Directory with Field API type information                                                             --  types-fieldapi',
  'types-fieldapi-non-blocked=s@   -- Non-blocked data types (without NPROMA)                                                               --  CPG_SL1F_TYPE,CPG_SL_MASK_TYPE',
  'use-acpy                        -- Avoid pointer aliasing using ACPY                                                                     ',
  'use-bcpy                        -- Avoid pointer aliasing using BCPY                                                                     ',
  'version                         -- Append fxtran-acdc version at end of generated content                                                ',
);
sub parallel
{
  my ($opts, @args) = @_;
  print &Dumper ([$opts, @args]);
}

&click
(
  'copy                            -- Generate COPY method                                                                                  ',
  'crc64                           -- Generate CRC64 method                                                                                 ',
  'dir=s                           -- Dump results in this directory                                                                        --  .',
  'field-api                       -- Dump Field API information                                                                            ',
  'field-api-class=s               -- Field API structure category                                                                          ',
  'host                            -- Generate HOST method                                                                                  ',
  'legacy                          -- Generate LEGACY method                                                                                ',
  'load                            -- Generate LOAD method                                                                                  ',
  'module-map=s%                   -- Type/module mapping for methods                                                                       ',
  'no-allocate=s@                  -- Structures that should not be allocated/deallocated                                                   ',
  'only-components=s               -- Process only these derived type members                                                               ',
  'only-types=s                    -- Process only these derived types                                                                      ',
  'out                             -- Output file name                                                                                      ',
  'pragma                          -- Pragma (OpenACC or OpenMP)                                                                            --  OpenACC',
  'save                            -- Generate SAVE method                                                                                  ',
  'size                            -- Generate SIZE method                                                                                  ',
  'skip-components=s               -- Skip these derived type members                                                                       ',
  'skip-types=s                    -- Skip these derived types                                                                              ',
  'sorted                          -- Sort files (with number prefix) in compilation order                                                  ',
  'tmp=s                           -- Temporary directory for ancillary files                                                               ',
  'type-bound-methods              -- Generate & use type bound methods                                                                     ',
  'types-constant-dir=s            -- Directory with constant type information                                                              --  types-constant',
  'types-fieldapi-dir=s            -- Directory with Field API type information                                                             --  types-fieldapi',
  'wipe                            -- Generate WIPE method                                                                                  ',
);
sub methods
{
  my ($opts, @args) = @_;
  print &Dumper ([$opts, @args]);
}

1;
