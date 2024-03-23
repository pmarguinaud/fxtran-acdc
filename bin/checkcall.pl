#!/usr/bin/perl -w

use strict;
use local::lib;
use fxtran;
use fxtran::parser;
use fxtran::xpath;

use File::Basename;
use FileHandle;
use Data::Dumper;
use Memoize;

use lib "/home/gmap/mrpm/marguina/fxtran-acdc/lib";
use Bt;
use Finder::Pack;


sub center
{
  my ($str, $len) = @_;

  $len -= length ($str);

  my $len1 = int ($len / 2);
  my $len2 = $len - $len1;

  return (' ' x $len1) . $str . (' ' x $len2);
}


sub getDummies
{
  my $d = shift;

  my @arg = &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N', $d, 1);

  my @en_decl;

  if (@arg)
    {
      no warnings;

      my @AA = @arg;

      while (my @aa = splice (@AA, 0, 10))
        {
          my $xpath = './object/file/program-unit/T-decl-stmt//EN-decl[' . join (' or ', map { "string(EN-N)='$_'" } @aa) . ']';
          push @en_decl, &F ($xpath, $d);
        }

    }

  my %optional;

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &F ('ancestor::ANY-stmt', $en_decl);
      die unless ($stmt);
      my ($optional) = &F ('.//attribute-N[string(.)="OPTIONAL"]', $stmt, 1);
      $optional{$N} = $optional;
    }

  my @dummies;

  for my $arg (@arg)
    {
      push @dummies, {name => $arg, optional => $optional{$arg}};
    }

  return @dummies;
}

&memoize ('getDummies');

sub getDimensions 
{
  my ($d, $n) = @_;

  my ($as) = &F ('.//T-decl-stmt//EN-decl[string(EN-N)="?"]/array-spec', $n, $d, 1);

  if ($as)
    {
      for ($as)
        {
          s/YDGEOMETRY%YRDIM%NPROMA/KLON/go;
          s/YDGEOMETRY%YRDIM%NPROMM/KLON/go;
          s/YDCPG_OPTS%KLON/KLON/go;
          s/KPROMA/KLON/go;
          s/YDGEOMETRY%YRDIMV%NFLEVG/KFLEVG/go;
          s/YDCPG_OPTS%KFLEVG/KFLEVG/go;
          s/\bKLEV\b/KFLEVG/go;
          s/\b1://go;
          s/YDCPG_OPTS%KTSSG/KTSSG/go;
          s/YDCPG_OPTS%KSW/KSW/go;
          s/YDDIMV%NFLEVG/KFLEVG/go;
        }
    }

  return $as || '';
}

sub getIntent
{
  my ($d, $n) = @_;

  my ($intent) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]//intent-spec', $n, $d, 1);

  return $intent;
}

sub getActuals
{
  my ($call, @dummies) = @_;

  my @arg = &F ('./arg-spec/arg', $call);

  my $seen_kw = 0;

  my @actuals;

  for my $arg (@arg)
    {
      my @c = $arg->childNodes ();

      if ((scalar (@c) == 1) && (! $seen_kw))
        {
          my $dummy = shift (@dummies);
          push @actuals, {dummy => $dummy->{name}, value => $c[0]};
        }
      elsif ((scalar (@c) == 3) && ($c[1]->textContent =~ m/^\s*=\s*$/o))
        {
          $seen_kw++;
          push @actuals, {dummy => $c[0]->textContent, value => $c[2]};
        }
      else
        {
          die;
        }
    }

  return @actuals;
}

sub eqArgSubStruct
{
  my ($v1, $v2) = @_;
  if (($v1 =~ s/^YD/YR/o) && ($v2 =~ s/.*%//o))
    {
      return $v1 eq $v2;
    }
}

sub eqArgDummyActual
{
  my ($v1, $v2) = @_;
  if ($v2 =~ s/^YL/YD/o)
    {
      return $v1 eq $v2;
    }
  elsif ($v2 =~ s/^Z/P/o)
    {
      return $v1 eq $v2;
    }
  elsif ($v2 =~ s/^LL/LD/o)
    {
      return $v1 eq $v2;
    }
  elsif ($v2 =~ s/^I/K/o)
    {
      return $v1 eq $v2;
    }
}

sub eqArgOther
{
  my ($v1, $v2) = @_;
  my @x = 
qw (
LDNHDYN              YDMODEL%YRML_DYN%YRDYNA%LNHDYN
KFLEV                YDGEOMETRY%YRDIMV%NFLEVG
KPROMA               YDGEOMETRY%YRDIM%NPROMA
KST                  YDCPG_BNDS%KIDIA    
KSTART               YDCPG_BNDS%KIDIA    
KEND                 YDCPG_BNDS%KFDIA    
KPROF                YDCPG_BNDS%KFDIA    
K1                   YDCPG_BNDS%KIDIA    
K2                   YDCPG_BNDS%KFDIA    
KD                   YDCPG_BNDS%KIDIA    
KF                   YDCPG_BNDS%KFDIA    
KVDVAR               YDDYNA%NVDVAR       
KFLEV                NFLEVG              
KPROMA               NPROMA              
KLEV                 NFLEVG              
KSTART               KST
LDNHDYN              YDDYNA%LNHDYN
KPROF                KEND
KIDIA                YDCPG_BNDS%KIDIA    
KFDIA                YDCPG_BNDS%KFDIA    
KLON                 YDCPG_OPTS%KLON     
KLEV                 YDCPG_OPTS%KFLEVG   
KSTEP                YDCPG_OPTS%NSTEP    


);

  while (my ($a1, $a2) = splice (@x, 0, 2))
    {
      return 1 if (($v1 eq $a1) && ($v2 eq $a2));
    }
}

sub eqArg
{
  my ($v1, $v2) = @_;
  return 1 if ($v1 eq $v2);
  return 1 if (&eqArgSubStruct ($v1, $v2));
  return 1 if (&eqArgDummyActual ($v1, $v2));
  return 1 if (&eqArgOther ($v1, $v2));
}

sub checkcall
{
  my ($caller_d, $callee_d) = @_;

  my @dummies = &getDummies ($callee_d);
  my ($name) = &F ('././object/file/program-unit/subroutine-stmt/subroutine-N', $callee_d, 1);
  
  my @call = &F ('.//call-stmt[string(procedure-designator)="?"]', $name, $caller_d);
  
  for my $call (@call)
    {

      print $call->textContent, "\n";

      my @actuals = &getActuals ($call, @dummies);
  
      my %used;
  
      print &center ($name, 50), "\n";
      printf ("     | %20s %60s | %20s %60s\n", &center ('DUMMY', 20), '', &center ('ACTUAL', 20), '');
      for my $actual (@actuals)
        {
          $used{$actual->{dummy}}++ if ($actual->{dummy});
  
          my $dummy = $actual->{dummy} || '-';
          my $value = $actual->{value}->textContent;

          my $valueDims = &getDimensions ($caller_d, $value);
          my $dummyDims = &getDimensions ($callee_d, $dummy);
          my $dummyItnt = &getIntent     ($callee_d, $dummy);

          printf(" %s %s | %-20s %-60s | %-20s %-60s | %5s\n", 
                 ((($valueDims eq $dummyDims) or ((! $valueDims) or (! $dummyDims))) ? '=' : 'X'),
                 (&eqArg ($dummy, $value) ? '=' : ' '), 
                 $dummy, $dummyDims, $value, $valueDims, $dummyItnt);
        }
    
      for my $dummy (@dummies)
        {
          if ((! $dummy->{optional}) && (! $used{$dummy->{name}}))
            {
              print "$dummy->{name} is missing\n";
            }
        }
  
      for my $used (sort keys (%used))
        {
          unless (grep { $used eq $_->{name} } @dummies)
            {
              print "$used is used but does not exist\n";
            }
          if ($used{$used} > 1)
            {
              print "$used used more than once\n";
            }
        }

      print "\n" x 2;

    }

}

sub checkcalls
{
  my ($find, $caller_F90) = @_;

  $caller_F90 = $find->resolve (file => $caller_F90);

  die unless ($caller_F90);

  my @fopts = qw (-construct-tag -no-include -line-length 512);
  my $caller_d = &parse (location => $caller_F90, fopts => \@fopts, dir => "$ENV{HOME}/tmp");

  my @proc = &F ('.//procedure-designator', $caller_d, 1);
   
  for my $proc (@proc)
    {
      my $callee_F90 = $find->resolve (file => lc ($proc) . '.F90');

      unless ($callee_F90)   
        {
          print "$proc was not found\n";
          next;
        }

      my $callee_d = &parse (location => $callee_F90, fopts => \@fopts, dir => "$ENV{HOME}/tmp");
      
      &checkcall ($caller_d, $callee_d);
    }
}

my $find = 'Finder::Pack'->new ();

&checkcalls ($find, $ARGV[0]);

