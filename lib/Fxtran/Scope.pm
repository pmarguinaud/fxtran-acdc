package Fxtran::Scope;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;

sub getExec
{
  my $d = shift;
 
  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);

  my @anc = &F ('ancestor::*', $exec);

  for my $anc (reverse (@anc))
    {
      if ($anc->nodeName =~ m/-(?:construct|stmt)$/o)
        {
          $exec = $anc;
        }
    }

  return $exec;
}

sub getNoExec
{
  my $d = shift;
 
  my @stmt = &F ('.//ANY-stmt', $d); 

  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } @stmt;

  if (! $exec)
    {
      return $stmt[-2];
    }

  my @prev = &F ('preceding::*', $exec);

  my $prev;

  for my $p (reverse (@prev))
    {
      next if ($p->nodeName eq '#text');
      next if ($p->nodeName eq 'C');
      if ($p->nodeName eq 'ACDC-directive')
        {
          next if ($p->textContent =~ m/^PARALLEL/o);
        }
      next if ($p->nodeName eq 'ACDC');
      $prev = $p;
      last;
    }

  $prev or die $d->textContent;

  my @anc = &F ('ancestor::*', $prev);

  for my $anc (reverse (@anc))
    {
      if (($anc->nodeName =~ m/-(?:construct|stmt)$/o) || ($anc->nodeName eq 'include'))
        {
          $prev = $anc;
        }
    }

  return $prev;
}

sub removeWhiteSpaces
{
  my $d = shift;

  if ($d->isa ('XML::LibXML::Document'))
    {
      $d = $d->documentElement;
    }

  $d->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $d);

  for my $text (@text)
    {    
      if ($text->data =~ m/\n/goms)
        {
          $text->setData ("\n");
        }
    }    
}

sub removeWhiteLines
{
  my $d = shift;

  if ($d->isa ('XML::LibXML::Document'))
    {
      $d = $d->documentElement;
    }

  $d->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $d);

  for my $text (@text)
    {    
      my $tt = $text->data;
      if ($tt =~ m/\n/goms)
        {
          $tt =~ s/^.*\n([ ]*?)$/\n$1/goms;
          $text->setData ($tt);
        }
    }    
}


1;
