package Fxtran::BitRepro;

use strict;

use Fxtran::Intrinsic;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Module;
use Fxtran;

sub addParens
{
  my $expr = shift;
  my $par = &n ('<parens-E/>');
  my $p = $expr->parentNode;
  $p->replaceChild ($par, $expr);
  $par->appendChild ($_) for (&t ('('), $expr, &t (')'));
}

sub wrapMinusWithParens
{
  my $expr = shift;
  
  my ($e1) = &F ('./E-1/ANY-E', $expr);
  my ($e2) = &F ('./E-2/ANY-E', $expr);

  return unless ($e1 && $e2);

  my $sum = &e ('s1+s2'); my ($s1) = &F ('./E-1', $sum); my ($s2) = &F ('./E-2', $sum); 

  my $min = &e ('-m'); my ($m) = &F ('./E-1', $min);
  
  $min->replaceChild ($m, $e2);

  $sum->replaceChild ($s1, $e1);

  $sum->replaceChild ($s2, $min);

  $expr->replaceChild ($sum, $expr);
}

sub reorderPlusExpr
{
  my $expr = shift;

  my $p = $expr->parentNode;
  return unless (($p->nodeName eq 'op-E') && &F ('./op[string(.)="+"]', $p));
  my ($e1, $op, $e2, $e3) = $p->childNodes (); 
  return unless ($expr->unique_key eq $e2->unique_key);
  ($e2, $op, $e3) = $expr->childNodes (); 

  #            p                                        p
  #            |                                        |
  #      +-----+------expr          ->      expr--------+-----+
  #      |              |                     |               |            
  #      e1       +-----+-----+         +-----+-----+         e3
  #               |           |         |           |
  #               e2          e3        e1          e2
  
  $p->replaceChild ($expr, $e1);
  $p->appendChild ($e3);
  $expr->appendChild ($e2);
  $expr->insertBefore ($e1, $op);
}



sub makeBitReproducible
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;

      if ($kind eq 'module')
        {
          &processSingleModule ($pu, %opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &processSingleRoutine ($pu, %opts);
        }
      else
        {
          die;
        }

    }

}

sub processSingleModule
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./program-unit', $d))
    {
      &processSingleRoutine ($pu, %opts);
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-bitrepro'});

}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::Intrinsic::makeBitReproducible ($pu, %opts);

  my ($ep) = &F ('./execution-part', $pu);

  if ($opts{'use-bit-repro-parens'})
    {
      for my $expr (&F ('.//op-E[string(./op)="-"]', $ep))
        {
          &wrapMinusWithParens ($expr);
        }
      for my $expr (&F ('.//op-E[string(./op)="+"]', $ep))
        {
          &reorderPlusExpr ($expr);
        }
      for my $expr (&F ('.//op-E[string(./op)="+"]', $ep))
        {
          &addParens ($expr);
        }
    }

  &Fxtran::Call::addSuffix 
  (
    $pu,
    section => $ep,
    suffix => $opts{'suffix-bitrepro'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; ($proc ne 'ABOR1') && ($proc ne 'PRINT_MSG') },
    contained => 1,
  );

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-bitrepro'});

  for my $pu (&F ('./program-unit', $pu))
    {
      &processSingleRoutine ($pu, %opts);
    }

}

1;
