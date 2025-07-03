package Fxtran::Intrinsic;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;
use FileHandle;
use File::Basename;

use strict;

use Fxtran;
use Fxtran::Decl;
use Fxtran::Canonic;

my @BR = qw (SIN COS TAN ASIN ACOS ATAN SINH COSH TANH ASINH ACOSH ATANH LOG EXP ATAN2);

my %BR = map { ($_, 1) } @BR;

my @INTRINSIC = qw (SIGN MAX MIN MOD REAL SQRT PRESENT INT NULL HUGE TINY EPSILON 
                    ABS TINY SUM NINT MERGE SIZE TRIM COUNT FLOAT GETENV);

my %INTRINSIC = map { ($_, 1) } (@INTRINSIC, @BR);

sub isIntrinsic
{
  my $s = shift;
  return $INTRINSIC{$s};
}

sub slurp
{
  my $file = shift;
  return do { my $fh = 'FileHandle'->new ("<$file"); local $/ = undef; <$fh> };
}

sub makeBitReproducibleSection
{
  my $s = shift;
  my %opts = @_;

  my $br = $opts{br};

  my $count = 0;

  for my $expr (&F ('.//named-E[./R-LT/function-R]', $s))
    {
      my ($n) = &F ('./N/n/text()', $expr);
      my $t = $n->textContent;
      next unless ($BR{$t});
      $n->setData ("BR_$t");
      $br->{"BR_$t"} = 1;
      $count++;
    }

  for my $expr (&F ('.//op-E[string(./op)="**"]', $s))
    {
      my ($op1, $op2) = &F ('./ANY-E', $expr);
      my $e = &n ('<named-E><N><n>BR_POW</n></N>' 
                . '<R-LT><function-R>(<element-LT>'
                . join (', ', map { '<element>' . $_ . '</element>' } ($op1, $op2))
                . '</element-LT>)</function-R></R-LT></named-E>');

      $expr->replaceNode ($e);

      $br->{'BR_POW'} = 1;
      $count++;
    }

  return $count;
}

sub makeBitReproducible
{ 
  my $pu = shift;
  my %opts = @_;

  my $find = $opts{find};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  my %br;

  &makeBitReproducibleSection ($ep, %opts, br => \%br);

  my @include = &F ('./include', $dp);

  for my $include (@include)
    {
      my ($ft) = &F ('./filename/text()', $include);
      my $filename = $ft->textContent;
      next unless ($filename =~ m/\.func.h$/o);

      $filename = $find->resolve (file => $filename);
      my $text = &slurp ($filename);
      my @stmt = grep { $_->nodeName =~ m/-stmt$/o }
                 &Fxtran::parse (fragment => $text, fopts => [qw (-line-length 500 -canonic)]);


      my $fh = 'FileHandle'->new ('>' . 'br_' . &basename ($filename));

      for my $stmt (@stmt)
        {
          &Fxtran::Canonic::makeCanonicReferences ($stmt);
          &makeBitReproducibleSection ($stmt, %opts, br => \%br);
          $fh->print ($stmt->textContent . "\n");
        }

      $fh->close ();

      $ft->setData ($filename);
    }

  if (%br)
    {
      &Fxtran::Decl::use ($pu, 'USE BR_INTRINSICS, ONLY : ' . join (', ', sort keys (%br)));
    }

}

1;
