#!/usr/bin/perl -w
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

use Fxtran;
use Decl;
use Pointer::Parallel::Object;
use Pointer::Parallel::SymbolTable;
use Pointer::Parallel;
use Loop;
use Call;
use Associate;
use Subroutine;
use Finder::Pack;
use Include;
use DIR;

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

sub parseDirectives
{

# Add tags for each section

  my $d = shift;

  my @C = &F ('//C[starts-with(string (.),"!=")]', $d);
  
  while (my $C  = shift (@C))
    {
      (my $bdir = $C->textContent) =~ s/^!=\s*//o;

      ($bdir, my $opts) = ($bdir =~ m/^(\w+)\s*(?:\(\s*(\S.*\S)\s*\)\s*)?$/goms);
      my %opts = $opts ? split (m/\s*[=,]\s*/o, $opts) : ();

      $opts{TARGET} ||= 'OpenMP';

      $bdir = lc ($bdir);
      my ($tag) = ($bdir =~ m/^(\w+)/o);
  
      my @node;
      for (my $node = $C->nextSibling; ; $node = $node->nextSibling)
        {
          $node or die $C->textContent;
          if (($node->nodeName eq 'C') && (index ($node->textContent, '!=') == 0))
            {
              my $C = shift (@C);
              (my $edir = $C->textContent) =~ s/^!=\s*//o;
              $edir = lc ($edir);

              die unless ($edir =~ s/^end\s+//o);
              die unless ($edir eq $tag);

              $C->unbindNode ();
              
              last;
            }

          push @node, $node;

        }

      my $e = &n ("<$tag-section " . join (' ', map { sprintf ('%s="%s"', lc ($_), $opts{$_}) } keys (%opts))  . "/>");
 
      for my $node (@node)
        {
          $e->appendChild ($node);
        }

      $C->replaceNode ($e);

    }
}

my $suffix = '_parallel';

my %opts = ('types-dir' => 'types', skip => 'PGFL,PGFLT1,PGMVT1,PGPSDT2D', nproma => 'YDCPG_OPTS%KLON');
my @opts_f = qw (help only-if-newer version);
my @opts_s = qw (skip nproma);

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
      join ('', map { "  --$_=...\n" } @opts_f) .
     "\n";
    exit (0);
  }

$opts{skip} = [split (m/,/o, $opts{skip} || '')];

my $F90 = shift;
(my $F90out = $F90) =~ s/.F90$/$suffix.F90/o;

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

my $find = 'Finder::Pack'->new ();

my $types = &Storable::retrieve ("$opts{'types-dir'}/decls.dat");

my $doc = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 300 -no-include -no-cpp -construct-tag)]);
&Subroutine::rename ($doc, sub { return $_[0] . uc ($suffix) });

# Prepare the code

&Associate::resolveAssociates ($doc);

&Decl::forceSingleDecl ($doc);

&parseDirectives ($doc);

# Add modules

&Decl::use ($doc, map { "USE $_" } qw (FIELD_MODULE FIELD_REGISTRY_MOD FIELD_HELPER_MODULE YOMPARALLELMETHOD STACK_MOD));

# Add local variables

&Decl::declare 
($doc,  
  'INTEGER(KIND=JPIM) :: JBLK',
  'TYPE(CPG_BNDS_TYPE) :: YLCPG_BNDS', 
  'REAL(KIND=JPRB) :: ZHOOK_HANDLE_FIELD_API',
  'REAL(KIND=JPRB) :: ZHOOK_HANDLE_PARALLEL',
  'TYPE(STACK) :: YLSTACK',
);

my $t = &Pointer::Parallel::SymbolTable::getSymbolTable 
  ($doc, skip => $opts{skip}, nproma => $opts{nproma}, 'types-dir' => $opts{'types-dir'});

for my $v (qw (JLON JLEV))
  {
    &Decl::declare ($doc, "INTEGER(KIND=JPIM) :: $v") unless ($t->{$v});
  }

# Remove SKIP sections

for (&F ('.//skip-section', $doc))
  {
    $_->unbindNode ();
  }

# Transform NPROMA fields into a pair of (FIELD API object, Fortran pointer)

&Pointer::Parallel::fieldifyDecl ($doc, $t);

# Process parallel sections

my @par = &F ('.//parallel-section', $doc);


for my $ipar (0 .. $#par)
  {
    my $par = $par[$ipar];
    my $name = $par->getAttribute ('name') || $ipar;
    &Pointer::Parallel::makeParallel ($par, $t, $find, $types, "$NAME:$name");
  }

# Process call to parallel routines

my @call = &F ('.//call-stmt[not(ancestor::parallel-section)]' # Skip calls in parallel sections
            . '[not(string(procedure-designator)="DR_HOOK")]'  # Skip DR_HOOK calls
            . '[not(procedure-designator/named-E/R-LT)]'       # Skip objects calling methods
            . '[not(ancestor::serial-section)]', $doc);        # Skip calls in serial sections

my %seen;

for my $call (@call)
  {
    if (&Pointer::Parallel::callParallelRoutine ($call, $t))
      {
        # Add include for the parallel CALL
        my ($name) = &F ('./procedure-designator/named-E/N/n/text()', $call);
        unless ($seen{$name->textContent}++)
          {
            my ($include) = &F ('.//include[./filename[string(.)="?"]]', lc ($name) . '.intfb.h', $doc);
            $include->parentNode->insertAfter (&n ('<include>#include "<filename>' . lc ($name) . '_parallel.intfb.h</filename>"</include>'), $include);
            $include->parentNode->insertAfter (&t ("\n"), $include);
          }
        $name->setData ($name->data . uc ($suffix));
      }
  }

# Declare pointers required for parallel sections

my @decl;

for my $n (sort keys (%$t))
  {
    my $s = $t->{$n};
    next unless ($s->{object_based});
    my $decl = &s ($s->{ts}->textContent . ", POINTER :: " . $n . "(" . join (',', (':') x ($s->{nd} + 1)) . ")");
    push @decl, $decl;
  }


&Decl::declare($doc, @decl);

# Create/delete fields for local arrays

&Pointer::Parallel::setupLocalFields ($doc, $t, '');

&Include::removeUnusedIncludes ($doc);



for my $ipar (0 .. $#par)
  {
    my $par = $par[$ipar];

    my $target = $par->getAttribute ('target');
    my $name = $par->getAttribute ('name') || $ipar;
    my @target = split (m/\//o, $target);

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

    $par->replaceNode ($if_construct);

    my @block;
    
    for my $itarget (0 .. $#target)
      {
        my $target = $target[$itarget];

        my $class = "Pointer::Parallel::$target";

        eval "use $class";
        if (my $c = $@)
          {
            die $c;
          }
        
        my $par1 = $class->makeParallel ($par->cloneNode (1), $t);
        
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

        $C->replaceNode ($par1);

        push @block, $block;
      }

    $if_construct->parentNode->insertBefore (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',0,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
    $if_construct->parentNode->insertBefore (&t ("\n"), $if_construct);


    $if_construct->parentNode->insertAfter (&s ("IF (LHOOK) CALL DR_HOOK ('$NAME:$name',1,ZHOOK_HANDLE_PARALLEL)"), $if_construct);
    $if_construct->parentNode->insertAfter (&t ("\n"), $if_construct);

  }


my @called = &F ('.//call-stmt/procedure-designator', $doc, 1);

my @include = &F ('.//include', $doc);

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

if (@par)
  {
    # Add abor1.intfb.h

    unless (&F ('.//include[string(filename)="abor1.intfb.h"]', $doc))
      {
        &Include::addInclude ($doc, 'abor1.intfb.h');
      }
  }

# include stack.h

my ($implicit) = &F ('.//implicit-none-stmt', $doc);

$implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
$implicit->parentNode->insertBefore (&t ("\n"), $implicit);

if ($opts{version})
  {
    my $version = &Fxtran::getVersion ();
    my ($file) = &F ('./object/file', $doc);
    $file->appendChild (&n ("<C>! $version</C>"));
    $file->appendChild (&t ("\n"));
  }


&updateFile ($F90out, $doc->textContent);



