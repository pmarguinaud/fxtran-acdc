package Directive;

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

  my @C = &F ("//$name-directive", $d);
  
  while (my $C  = shift (@C))
    {
      my $bdir = $C->textContent;
      if ($bdir =~ m/^(?:pointerparallel|openacc|methods)/io)
        {
          $C->unbindNode ();
          next;
        }

      my $noend = ! ($bdir =~ s/\s*{\s*$//o);

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
      $Tag .= '-section' unless ($noend);

      my $e = &n ("<$Tag " . join (' ', map { sprintf ('%s="%s"', lc ($_), $opts{$_}) } keys (%opts))  . "/>");

      if (! $noend)
        {
          my @node;
          for (my $node = $C->nextSibling; ; $node = $node->nextSibling)
            {
              $node or die &Dumper ([$C->textContent, map { $_->textContent } @node]);
              if ($node->nodeName eq "$name-directive")
                {
                  my $C = shift (@C);
                  die unless ($C->unique_key eq $node->unique_key);
                  my $edir = $C->textContent;
                  die &Dumper ([map { $_->textContent } @node]) unless ($edir =~ m/\s*}\s*/o);

                  $C->unbindNode ();
                  
                  last;
                }
              push @node, $node;
            }

          for my $node (@node)
            {
              $e->appendChild ($node);
            }
        }

      $C->replaceNode ($e);
      push @e, $e;

    }

  return @e;
}


1;
