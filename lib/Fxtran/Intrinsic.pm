package Fxtran::Intrinsic;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;
use FileHandle;
use File::Basename;
use File::Temp;

use strict;

use Fxtran;
use Fxtran::Decl;
use Fxtran::Canonic;

my @BR = qw (SIN COS TAN ASIN ACOS ATAN SINH COSH TANH ASINH ACOSH ATANH LOG LOG10 EXP ERF ERFC GAMMA LOG_GAMMA ATAN2);

my %BR = map { ($_, 1) } @BR;

my @INTRINSIC = qw (SIGN MAX MIN MOD REAL SQRT PRESENT INT NULL HUGE TINY EPSILON ANY ALLOCATED
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

# Code to replace intrinsic with their BR version

sub makeBitReproducibleSection
{
  my $s = shift;
  my %opts = @_;

  my $brlist = $opts{brlist};

  my $count = 0;

  for my $expr (&F ('.//named-E[./R-LT/function-R]', $s))
    {
      my ($n) = &F ('./N/n/text()', $expr);
      my $t = $n->textContent;
      next unless ($BR{$t});
      $n->setData ("FXTRAN_ACDC_BR_$t");
      $brlist->{"FXTRAN_ACDC_BR_$t"} = 1;
      $count++;
    }

  for my $expr (&F ('.//op-E[string(./op)="**"]', $s))
    {
      my ($op1, $op2) = &F ('./ANY-E', $expr);
      my $e = &n ('<named-E><N><n>FXTRAN_ACDC_BR_POW</n></N>' 
                . '<R-LT><function-R>(<element-LT>'
                . join (', ', map { '<element>' . $_ . '</element>' } ($op1, $op2))
                . '</element-LT>)</function-R></R-LT></named-E>');

      $expr->replaceNode ($e);

      $brlist->{'FXTRAN_ACDC_BR_POW'} = 1;
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

  my $brlist = $opts{brlist} || {};

  &makeBitReproducibleSection ($ep, %opts, brlist => $brlist);

  for my $include (&F ('./include', $dp))
    {
      my ($ft) = &F ('./filename/text()', $include);
      my $filename = $ft->textContent;

      next unless ($filename =~ m/\.func.h$/o);

      $filename = $find->resolve (file => $filename);
      my $text = &slurp ($filename);

      my ($prog) = &Fxtran::parse (program => "PROGRAM MAIN\n${text}\nEND\n", fopts => [qw (-line-length 500 -construct-tag)]);

      &Fxtran::Canonic::makeCanonicReferences ($prog);

      next unless (&makeBitReproducibleSection ($prog, %opts, brlist => $brlist));

      $filename = 'br_' . &basename ($filename);

      my $fh = 'FileHandle'->new (">$filename");

      for ($prog->firstChild, $prog->lastChild)
        {
          $_->unbindNode ();
        }

      $fh->print ($prog->textContent);

      $fh->close ();

      $ft->setData ($filename);
    }

  if (my ($contains) = &F ('./contains-stmt', $pu))
    {
      for my $include (&F ('following-sibling::include-stmt', $contains))
        {
          my ($ft) = &F ('./filename/S/text()', $include);
          my $filename = $ft->textContent;

          $filename =~ s/(?:^['"]|['"]$)//goms;

          $filename = $find->resolve (file => $filename);

          my $text = &slurp ($filename);

          my $fh = 'File::Temp'->new (SUFFIX => '.F90');
          $fh->print ($text);
          $fh->close ();

          my ($prog) = &Fxtran::parse (location => $fh->filename (), fopts => [qw (-line-length 500 -construct-tag)]);

          &Fxtran::Canonic::makeCanonicReferences ($prog);
          &Fxtran::Canonic::makeCanonic ($prog, %opts);

          &Fxtran::BitRepro::makeBitReproducible ($prog, %opts, brlist => $brlist, contained => 1);

          $filename = 'br_' . &basename ($filename);

          $fh = 'FileHandle'->new (">$filename");

          $fh->print ($prog->textContent);

          $fh->close ();

          $ft->setData ('"' . $filename . '"');
        }
    }

  if ((%$brlist) && (! $opts{contained}))
    {
      &Fxtran::Decl::use ($pu, 'USE FXTRAN_ACDC_BR_INTRINSICS, ONLY : ' . join (', ', sort keys (%$brlist)));
    }

}

1;
