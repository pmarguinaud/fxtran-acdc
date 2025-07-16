package Fxtran::Directive;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Data::Dumper;

sub parseDirectives
{
# Add tags for each section

  my $d = shift;
  my %args = @_;

  my $name = $args{name};

  for my $n (&F (".//$name", $d))
    {
      my $t = $n->nextSibling;
      if (($t->nodeName eq '#text') && ($t->data =~ m/^\s+/o))
        {
          $t->unbindNode ();
        }
      $n->unbindNode ();
    }

  my @e;

  my @section;

  for my $C (&F ("//$name-directive", $d))
    {
      my $bdir = $C->textContent;

      if ($bdir =~ s/(?:\s*}\s*$|^END)//io) # Close section
        {
          my $Cc = $C;

          unless (@section)
            {
              die ("Unexpected `" . $Cc->textContent . "' in:\n" . $C->parentNode->textContent) 
            }
  
          my ($Co, $e, $tag) = @{ pop @section };

          unless ($Co->parentNode->unique_key == $Cc->parentNode->unique_key)
            {
              die ("Unexpected `" . $Cc->textContent . "' in:\n" . $C->parentNode->textContent);
            }
          
          for my $n (&F ('following-sibling::node()', $Co))
            {
              $n->unbindNode ();
              last if ($n->unique_key eq $Cc->unique_key);
              $e->appendChild ($n);
            }

          $Co->replaceNode ($e);

          push @e, $e;
        }
      else # Open section or one liner
        {
          my $open = $bdir =~ s/(?:\s*{\s*$|^BEGIN)//o;

          my %opts;

          my @bdir = split (m/\s*,\s*/o, $bdir);

          $bdir = lc (shift (@bdir));

          for my $s (@bdir)
            {
              my ($k, $v) = split (m/\s*=\s*/o, $s);
              $opts{$k} = $v;
            }

          my ($tag) = ($bdir =~ m/^(\w+)/o);
          my $Tag = $tag; 

          $Tag .= '-section';

          my $e = &n ("<$Tag " . join (' ', map { sprintf ('%s="%s"', lc ($_), $opts{$_}) } keys (%opts))  . "/>");

          if ($open)
            {
              push @section, [$C, $e, $tag];
            }
          else
            {
              $C->replaceNode ($e);
            }
        }

    }

  for (@section)
    {
      die ("Section " . $_->textContent . " was not closed");
    }

  return @e;
}

sub openmpToACDC 
{
  my ($d, %opts) = @_;

  for my $p (&F ('.//parallel-openmp|.//parallel-do-openmp|.//end-parallel-openmp|.//end-parallel-do-openmp', $d))
    {
      my $nn = $p->nodeName;

      if (my $pp = $p->previousSibling)
        {
          $pp->unbindNode () if ($pp->nodeName eq '#text');
        }

      $p->replaceNode (my $acdc = &n ('<ACDC>!$ACDC</ACDC>'));

      if (($nn eq 'parallel-openmp') || ($nn eq 'parallel-do-openmp'))
        {
          $acdc->parentNode->insertAfter ($_, $acdc) for (&n ('<ACDC-directive>PARALLEL{</ACDC-directive>', &t (' ')));
        }
      elsif (($nn eq 'end-parallel-openmp') || ($nn eq 'end-parallel-do-openmp'))
        {
          $acdc->parentNode->insertAfter ($_, $acdc) for (&n ('<ACDC-directive>}</ACDC-directive>', &t (' ')));
        }
      else
        {
          die $p;
        }
    }

  for my $omp (&F ('.//omp|.//ANY-openmp', $d))
    {
      if (my $n = $omp->nextSibling ())
        {
          $n->unbindNode () if ($n->nodeName eq '#text');
        }
      $omp->unbindNode ();
    }

}


1;
