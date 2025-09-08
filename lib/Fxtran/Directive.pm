package Fxtran::Directive;

=head1 NAME

Fxtran::Directive

=head1 DESCRIPTION

This module provides functions to parse ACDC directives.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;

sub parseDirectives
{

=head2 parseDirectives

Parse ACDC directives, such as:

  !$ACDC PARALLEL {

  !$ACDC }

and:

  !$ACDC singlecolumn

=cut

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

          if (grep { (! defined ($_)) || (m/^--/o) } %opts)
            {
              %opts = ();
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

# for my $par (&F ('.//parallel-section', $d))
#   {
#     if (my @par = &F ('ancestor::parallel-section', $par))
#       {
#         push @par, $par;

#         my $mess = "Nested parallel sections are not allowed:\n";
#
#         for (@par)
#           {
#             $mess .= " - " . $_->textContent . "\n";
#           }

#         die ($mess);
#       }
#   }

  return @e;
}

sub openmpToACDC 
{

=head2 openmpToACDC

Convert C<OpenMP> directives to ACDC parallel sections. The document has to be parsed
with C<OpenMP> directives parsing enabled.

=cut

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

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;
