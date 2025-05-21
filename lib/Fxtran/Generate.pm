package Fxtran::Generate;

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
  ydcpg_opts                -- Change KIDIA, KFDIA -> YDCPG_OPTS, YDCPG_BNDS
EOF

  my @options;

  for (split (m/\n/o, $options))
    {
      my ($opt) = (m/^\s*([\w-]+)/o);
      push @options, $opt, "$_\n";
    }

  @options
};

&click (<< "EOF");
@options{qw (cycle dir only-if-newer merge-interfaces pragma stack84 style redim-arguments set-variables 
             suffix-singlecolumn tmp value-attribute version inline-contained)}
  keep-drhook               -- Keep DrHook
  dummy                     -- Generate a dummy routine (strip all executable code)
  inlined=s@                -- List of routines to inline
  inline-comment            -- Add a comment when inlining a routine
  create-interface          -- Generate an interface file
  process-interfaces        -- Transform interfaces into single column interfaces (used for MODI MESONH files)
  no-check-pointers-dims=s@ -- List of pointer variables that should not be checked for their dimensions
  process-pointers          -- Process pointers (change them to CRAY pointers
EOF
sub singlecolumn
{
  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::SingleColumn');

  my ($F90) = @args;

  if ('File::Spec'->rel2abs ($opts->{dir}) ne 'File::Spec'->rel2abs (&dirname ($F90)))
    {
      &copy ($F90, join ('/', $opts->{dir}, &basename ($F90)));
    }
  
  my $suffix = lc ($opts->{'suffix-singlecolumn'});
  (my $F90out = $F90) =~ s/\.F90/$suffix.F90/;
  $F90out = $opts->{dir} . '/' . &basename ($F90out);
  $F90out = 'File::Spec'->rel2abs ($F90out);

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
  
  &Fxtran::Canonic::makeCanonic ($d, %$opts);
  
  $opts->{style} = 'Fxtran::Style'->new (%$opts, document => $d);

  $opts->{pragma} = 'Fxtran::Pragma'->new (%$opts);
  
  my $find = 'Fxtran::Finder'->new (files => $opts->{files}, base => $opts->{base}, I => $opts->{I});
  
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
          &Fxtran::SingleColumn::processSingleModule ($pu, $find, %$opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &Fxtran::SingleColumn::processSingleRoutine ($pu, $find, %$opts);
        }
      else
        {
          die;
        }
    }
  
  &Fxtran::Util::addVersion ($d)
    if ($opts->{version});
  
  &mkpath (&dirname ($F90out));
  'FileHandle'->new (">$F90out.xml")->print ($d->toString);
  &Fxtran::Util::updateFile ($F90out, &Fxtran::Canonic::indent ($d));

  if ($opts->{'create-interface'} && $singleRoutine)
    {
      $opts->{style}->generateInterface ($F90out, %$opts);
    }
}


&click (<< "EOF");
@options{qw (cycle dir tmp only-if-newer merge-interfaces pragma stack84 style redim-arguments ydcpg_opts
             suffix-singlecolumn suffix-pointerparallel version type-bound-methods types-constant-dir types-fieldapi-dir)}
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
EOF
sub pointerparallel
{
  my ($opts, @args) = @_;

  &Fxtran::Util::loadModule ('Fxtran::Pointer::Parallel');
  &Fxtran::Util::loadModule ('Fxtran::IO::Link');

  my ($F90) = @args;

  $opts->{pragma} = 'Fxtran::Pragma'->new (%$opts);

  $opts->{dir} = 'File::Spec'->rel2abs ($opts->{dir});
  
  if ($opts->{dir} ne 'File::Spec'->rel2abs (&dirname ($F90)))
    {
      &copy ($F90, join ('/', $opts->{dir}, &basename ($F90)));
    }

  (my $F90out = $F90) =~ s{.F90$}{lc ($opts->{'suffix-pointerparallel'}) . '.F90'}eo;
  
  $F90out = 'File::Spec'->catpath ('', $opts->{dir}, &basename ($F90out));
  
  if ($opts->{'only-if-newer'})
    {
      my $st = stat ($F90);
      my $stout = stat ($F90out);
      if ($st && $stout)
        {
          exit (0) unless ($st->mtime > $stout->mtime);
        }
    }
  
  my $find = 'Fxtran::Finder'->new (files => $opts->{files}, base => $opts->{base}, I => $opts->{I});
  
  my $linkTypes = &Fxtran::IO::Link::link ('types-fieldapi-dir' => $opts->{'types-fieldapi-dir'});
  my $types = $linkTypes->{decls};
  
  &fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 512));
  
  my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 5000 -no-include -no-cpp -construct-tag -directive ACDC -canonic)], dir => $opts->{tmp});
  
  &Fxtran::Canonic::makeCanonic ($d, %$opts);
  
  &Fxtran::Directive::parseDirectives ($d, name => 'ACDC');
  
  $opts->{style} = 'Fxtran::Style'->new (%$opts, document => $d);

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
  
  for my $pu (@pu)
    {
      &Fxtran::Pointer::Parallel::processSingleRoutine ($pu, $NAME, $find, $types, %$opts);
    }
  
  &Fxtran::Util::addVersion ($d)
    if ($opts->{version});
  
  &Fxtran::Util::updateFile ($F90out, &Fxtran::Canonic::indent ($d));


  if ($opts->{'parallelmethod-section'})
    {
      &Fxtran::Util::loadModule ('Fxtran::Generate::ParallelMethod');
      &Fxtran::Generate::ParallelMethod::generateCCode ($d, $opts);
    }
}


&click (<< "EOF");
@options{qw (dir pragma tmp type-bound-methods types-constant-dir types-fieldapi-dir)}
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

  if ($opts->{'type-bound-methods'} && (&dirname ($F90) eq $opts->{dir}))
    {
      die ("Dumping code in `$opts->{dir}` would overwrite `$F90'");
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
  
  my $doc = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 800)], dir => $opts->{tmp});
  
  if ($opts->{load} || $opts->{save} || $opts->{size} || $opts->{copy} || $opts->{host} || $opts->{crc64} || $opts->{legacy})
    {
      &Fxtran::IO::processTypes ($doc, $opts);
    }
  
  if ($opts->{'field-api'})
    {
      &Fxtran::FieldAPI::Register::registerFieldAPI ($doc, $opts);
    }
}

&click (<< "EOF");
@options{qw (dir pragma tmp merge-interfaces suffix-singlecolumn suffix-pointerparallel ydcpg_opts)}
EOF
sub interface
{
  my ($opts, @args) = @_;

  my ($F90) = @args;

  my $ext = '.intfb.h';

  my @D = @{ $opts->{D} };
  my $doc = &Fxtran::parse (location => $F90, fopts => [@D, '-construct-tag', '-no-include', '-line-length' => 500], dir => $opts->{tmp});

  my @text = split (m/\n/o, $doc->textContent);
  
  &Fxtran::Interface::intfbBody ($doc);

  my %intfb;
  
  # Strip empty lines
  
  $intfb{regular} = $doc->textContent ();
  $intfb{regular} =~ s/^\s*\n$//goms;

  &Fxtran::Util::loadModule ('Fxtran::Generate::Interface');

  for my $method (qw (singlecolumn pointerparallel))
    {
      &Fxtran::Generate::Interface::interface ($doc, \@text, $opts, \%intfb, $method);
    }

  my $sub = &basename ($F90, qw (.F90));
  
  &Fxtran::Util::updateFile ("$opts->{dir}/$sub$ext", << "EOF");
INTERFACE
$intfb{regular}
$intfb{singlecolumn}
$intfb{pointerparallel}
END INTERFACE
EOF

}

1;
