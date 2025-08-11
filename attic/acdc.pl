#!/usr/bin/perl -w

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FileHandle;

use strict;

use FindBin qw ($Bin);
use lib "$Bin/../lib";
use local::lib;

use Fxtran;
use Fxtran::Directive;
use Fxtran::Canonic;
use Fxtran::Style;

my $F90 = shift;

my $d = &Fxtran::parse (location => $F90, fopts => [qw (-line-length 5000 -no-include -no-cpp -construct-tag -canonic -directive ACDC)]);

my $style = 'Fxtran::Style'->new (document => $d);

&Fxtran::Canonic::makeCanonic ($d, cycle => 50);

&Fxtran::Directive::parseDirectives ($d, name => 'ACDC');

'FileHandle'->new (">d.$F90")->print (&Fxtran::Canonic::indent ($d));

my $D = $d->cloneNode (1);

for my $par (&F ('.//parallel-section', $D))
  {
    my $p = $par->parentNode;
    $p->insertBefore ($_, $par) for (&t ("\n"), &n ("<C>!\$ACDC PARALLEL {</C>"), &t ("\n"));
    for my $n ($par->childNodes ())
      {
        $p->insertBefore ($n, $par);
      }
    $p->insertBefore ($_, $par) for (&t ("\n"), &n ("<C>!\$ACDC }</C>"), &t ("\n"));
    $par->unbindNode ();
  }

'FileHandle'->new (">D.$F90")->print (&Fxtran::Canonic::indent ($D));


for my $par (&F ('.//parallel-section', $d))
  {
    my $p = $par->parentNode;
    for my $n ($par->childNodes ())
      {
        $p->insertBefore ($n, $par);
      }
    $par->unbindNode ();
  }

'FileHandle'->new (">0.$F90")->print (&Fxtran::Canonic::indent ($d));

my ($pu) = &F ('./object/file/program-unit', $d);


my ($dp) = &F ('./specification-part/declaration-part', $pu);
my ($ep) = &F ('./execution-part', $pu);

my %vars;

for my $nproma ($style->nproma ())
  {
    my @n = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[./array-spec/shape-spec-LT/shape-spec[string(.)="?"]]/EN-N', $nproma, $dp, 1);
    $vars{$_}++ for (@n);
  }

my $vars = join ('/', '', (sort keys (%vars)), '');

my @assign = &F ('.//a-stmt[./E-1/named-E[contains("?",concat("/",string(N),"/"))] or ./E-2/named-E[contains("?",concat("/",string(N),"/"))]]', $vars, $vars, $ep);

my $jlon = $style->jlon ();

my @it = ($style->jlon (), $style->jlev (), 'JN');
my %it = map { ($_, 1) } @it;

my %do;

my @array;

ASSIGN: for my $assign (@assign)
  {
    my @p = &F ('ancestor::*', $assign);

    for my $p (@p)
      {
        next ASSIGN if ($p->nodeName eq 'parallel-section');
      }

    for my $p (@p)
      {
        next unless ($p->nodeName eq 'do-construct');
        my ($v) = &F ('./do-stmt/do-V', $p, 1);
        if ($it{$v})
          {
            $do{$p->unique_key} = $p;
            next ASSIGN;
          }
      }

    # No loop was found, this is array syntax

    push @array, $assign;
  }

sub mergeParallel
{
  my ($par1, $par2) = @_;

  for (my $p = $par1->nextSibling; $p; $p = $p->nextSibling)
    {
      if ($p->unique_key eq $par2->unique_key)
        {
          for ($par2->childNodes ())
            {
              $par1->appendChild ($_);
            }
          $par2->unbindNode ();
          return 1;
        }
      elsif ($p->nodeName ne '#text')
        {
          return;
        }
    }
}

if (0) {
for my $do (values (%do))
  {
    my $p = $do->parentNode;
    $p->insertBefore ($_, $do) for (&t ("\n"), &n ("<C>!\$ACDC PARALLEL {</C>"), &t ("\n"), &t ("\n"));
    $p->insertAfter ($_, $do) for (&t ("\n"), &n ("<C>!\$ACDC }</C>"), &t ("\n"), &t ("\n"));
  }
} else {

for my $do (values (%do), @array)
  {
    my $par = &n ('<parallel-section/>');
    my $p = $do->parentNode;
    $p->replaceChild ($par, $do);
    $par->appendChild ($_) for ($do, &t ("\n"));
  }

my @par = &F ('.//parallel-section', $ep);

=pod

my %count;

for my $par (@par)
  {
    $count{$par->unique_key} = &F ('.//ANY-stmt', $par);
  }


my @ppar;

my %seen;

while (my $par = shift (@par))
  {
    next if ($seen{$par->unique_key}++);
    push @ppar, (my $ppar = [$par]);
    for my $s (&F ('./following-sibling::*', $par))
      {
        next if ($s->nodeName eq '#text');
        last unless ($s->nodeName eq 'parallel-section');
        push @$ppar, $s;
        $seen{$s->unique_key}++;
      }
  }

for my $ppar (@ppar)
  {
    print &Dumper ([map { $_->textContent } @$ppar]);
  }

=cut

for (my $i = 0; $i < scalar (@par)-1; $i++)
  {
    my @count0 = &F ('.//ANY-stmt', $par[$i+0]);
    my @count1 = &F ('.//ANY-stmt', $par[$i+1]);
    next if (scalar (@count0) + scalar (@count1) > 30);
    &mergeParallel ($par[$i+0], $par[$i+1]) && do { splice (@par, $i+1, 1); $i--; }
  }

}



'FileHandle'->new (">A.$F90")->print (&Fxtran::Canonic::indent ($d));
'FileHandle'->new (">A.$F90.xml")->print ($d->toString);

