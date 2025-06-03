package Fxtran::SingleBlock;

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
use Fxtran;

sub processSingleRoutine
{
  my ($pu, $find, %opts) = @_;

  my $style = $opts{style};
  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @nproma = $style->nproma ();
  
  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  
  my $var2dim = &Fxtran::Loop::getVarToDim ($pu, style => $style);
  
  {
  my @type = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);
  my %type = map { ($_, 1) } @type;
  
  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my %arg = map { ($_, 1) } @arg;
  
  my @present = grep { $var2dim->{$_} || $type{$_} } @arg;
  my @create = grep { ! $arg{$_} } sort (keys (%$var2dim));
  
  $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create);
  }
  
  my @par = &F ('.//parallel-section', $pu);
  
  for my $par (@par)
    {
      &Fxtran::Loop::removeNpromaLoopsInSection
      (
        $par, 
        style => $style, 
        var2dim => $var2dim,
      );
     
      my ($do) = &fxtran::parse (fragment => << "EOF");
DO $jlon = $kidia, $kfdia
ENDDO
EOF
  
      for my $x ($par->childNodes ())
        {
          $do->insertBefore ($x, $do->lastChild);
        }
  
      $par->replaceNode ($do);    
  
      if (&Fxtran::Stack::addStackInSection ($do))
        {
          &Fxtran::Stack::iniStackSingleBlock ($do, stack84 => 1);
        }
  
      my %priv;
      for my $expr (&F ('.//named-E', $do))
        {
          my ($n) = &F ('./N', $expr, 1);
          next if ($var2dim->{$n});
          my $p = $expr->parentNode;
          $priv{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
        }
  
      &Fxtran::Call::addSuffix 
      (
        $pu,
        section => $do,
        suffix => $opts{'suffix-singlecolumn'},
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
  
      $pragma->insertParallelLoopGangVector ($do, PRIVATE => [sort (keys (%priv))], COLLAPSE => [2]);
  
    }
  
  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-singleblock'},
    'merge-interfaces' => 1,
    match => sub { my $proc = shift; ! ($proc =~ m/$opts{'suffix-singlecolumn'}$/i) },
  );
  
  &Fxtran::Subroutine::addSuffix ($pu, '_SINGLEBLOCK');
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 

  &Fxtran::Decl::declare ($pu, 'TYPE (STACK) :: YLSTACK');
  &Fxtran::Decl::use ($pu, 'USE STACK_MOD');
}

1;
