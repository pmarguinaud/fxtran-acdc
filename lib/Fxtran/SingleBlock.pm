package Fxtran::SingleBlock;

use Data::Dumper;

use strict;

use Fxtran::Common;
use Fxtran::Loop;
use Fxtran::Stack;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Pragma;
use Fxtran::Finder;
use Fxtran::Style;
use Fxtran::Decl;
use Fxtran::Inline;
use Fxtran;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  my $find = $opts{find};

  for my $in (@{ $opts{inlined} })
    {
      my $f90in = $find->resolve (file => $in);
      my $di = &Fxtran::parse (location => $f90in, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)], dir => $opts{tmp});
      &Fxtran::Canonic::makeCanonic ($di, %opts);
      &Fxtran::Inline::inlineExternalSubroutine ($pu, $di, %opts);
    }
      

  # Process abort sections
  for my $abort (&F ('.//abort-section', $pu))
    {
      $abort->replaceNode (&s ("CALL ABOR1 ('NOT SUPPORTED')"));
    }

  my $style = $opts{style};
  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @nproma = $style->nproma ();
  
  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  
  # Arrays dimensioned with KLON and their dimensions

  my $var2dim = &Fxtran::Loop::getVarToDim ($pu, style => $style);
  
  {
  # Derived types, assume they are present on the device
  my @type = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);
  my %type = map { ($_, 1) } @type;
  
  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my %arg = map { ($_, 1) } @arg;
  
  my @present = grep { $var2dim->{$_} || $type{$_} } @arg;
  my @create = grep { ! $arg{$_} } sort (keys (%$var2dim));
  
  # Create local arrays, assume argument arrays are on the device
  $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create);
  }

  # Parallel sections

  my @par = &F ('.//parallel-section', $pu);

  for my $par (@par)
    {

      # Make the section single column

      &Fxtran::Loop::removeNpromaLoopsInSection
      (
        $par, 
        style => $style, 
        var2dim => $var2dim,
      );
     
      # Move section contents into a DO loop over KLON

      my ($do) = &fxtran::parse (fragment => << "EOF");
DO $jlon = $kidia, $kfdia
ENDDO
EOF
  
      for my $x ($par->childNodes ())
        {
          $do->insertBefore ($x, $do->lastChild);
        }
  
      $par->replaceNode ($do);    
  
      # Use a stack

      if (&Fxtran::Stack::addStackInSection ($do))
        {
          &Fxtran::Stack::iniStackSingleBlock ($do, stack84 => 1);
        }

      # Replace KIDIA/KFDIA by JLON in call statements

      for my $call (&F ('.//call-stmt', $do))
        {
          for my $var ($kidia,, $kfdia)
            {
              for my $expr (&F ('.//named-E[string(.)="?"]', $var, $call))
                {
                  $expr->replaceNode (&e ($jlon));
                }
            }
        }

      # Add single column suffix to routines called in this section

      &Fxtran::Call::addSuffix 
      (
        $pu,
        section => $do,
        suffix => $opts{'suffix-singlecolumn'},
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
  
      # Find private variables
      my %priv;
      for my $expr (&F ('.//named-E', $do))
        {
          my ($n) = &F ('./N', $expr, 1);
          next if ($var2dim->{$n});
          my $p = $expr->parentNode;
          $priv{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
        }

      # Add OpenACC directive  

      $pragma->insertParallelLoopGangVector ($do, PRIVATE => [sort (keys (%priv))]);
    }
  
  # Add single block suffix to routines not called from within parallel sections

  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-singleblock'},
    'merge-interfaces' => 1,
    match => sub 
    { 
      my $proc = shift; 

      return if ($proc eq 'ABOR1');

      if ($opts{'suffix-singlecolumn'})
        {
          return if ($proc =~ m/$opts{'suffix-singlecolumn'}$/i);
        }

      return 1;
    },
  );
  
  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-singleblock'});
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 
  &Fxtran::Decl::declare ($pu, 
                          'TYPE (STACK) :: YLSTACK',
                          'INTEGER :: ' . $jlon);

  &Fxtran::Decl::use ($pu, 'USE STACK_MOD');
}

1;
