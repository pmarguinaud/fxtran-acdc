#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

use Getopt::Long;
use FileHandle;
use Data::Dumper;
use File::Basename;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::Common;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Style;
use Fxtran::Decl;
use Fxtran::Pointer::Object;
use Fxtran::IO::Link;
use Fxtran::SingleColumn;
use Fxtran::Finder;
use Fxtran::Pragma;
use Fxtran::Directive;
use Fxtran::Loop;
use Fxtran::Stack;
use Fxtran::Subroutine;

my $linkTypes = &Fxtran::IO::Link::link ('types-fieldapi-dir' => 'types-fieldapi');
my $types = $linkTypes->{decls};

&fxtran::setOptions (qw (Fragment -construct-tag -no-include -line-length 5000));
&fxtran::setOptions (qw (Statement -line-length 5000));

my $F90 = shift;

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -directive ACDC -no-include -line-length 500 -canonic)]);

&Fxtran::Canonic::makeCanonic ($d, cycle => 49);
&Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

my $style = 'Fxtran::Style'->new (document => $d);
my $find = 'Fxtran::Finder'->new ();
my $pragma = 'Fxtran::Pragma'->new (pragma => 'OpenACC');

my ($pu) = &F ('./object/file/program-unit', $d);

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

my @par = &F ('.//parallel-section', $d);

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
      suffix => '_OPENACC',
      'merge-interfaces' => 1,
    );

    $pragma->insertParallelLoopGang ($do, PRIVATE => [sort (keys (%priv))]);

  }

&Fxtran::Call::addSuffix 
(
  $pu,
  suffix => '_SINGLEBLOCK',
  'merge-interfaces' => 1,
  match => sub { my $proc = shift; ! ($proc =~ m/_OPENACC$/io) },
);

&Fxtran::Subroutine::addSuffix ($pu, '_SINGLEBLOCK');

print &Fxtran::Canonic::indent ($d);


