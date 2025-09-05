package Fxtran::DetectParallel;

=head1 NAME

Fxtran::DetectParallel

=head1 DESCRIPTION

This module provides the routine C<createParallelSections> whose
purpose is to detect parallel loops (loops on NPROMA), and add
ACDC parallel sections.

Adjacent parallel sections can be merged if they do not contain
CALL statements and the number of statements involving calculations
do not exceed the C<max-statements-per-parallel> argument.

The C<parallel-iterator-list> contain the list of iterators
of loops that should be included in parallel sections (NPROMA and
vertical iterators are the default).

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran;

sub createParallelSections
{
  my ($pu, $var2dim, %opts) = @_;

  return unless ($opts{'max-statements-per-parallel'});

  my $style = $opts{style};

  my $ep;

  if ($pu->nodeName eq 'program-unit')
    {
      ($ep) = &F ('./execution-part', $pu);
    }
  else
    {
      $ep = $pu;
    }
  
  my $vars = join ('/', '', (sort keys (%$var2dim)), '');
  
  # All assignment statements involving NPROMA dimensioned arrays

  my @assign = &F ('.//a-stmt[./E-1/named-E[contains("?",concat("/",string(N),"/"))] or ./E-2/named-E[contains("?",concat("/",string(N),"/"))]]', $vars, $vars, $ep);
  
  my @it = ($style->jlon (), $style->jlev (), @{ $opts{'parallel-iterator-list'} });

  my %it = map { ($_, 1) } @it;
  
  my (%do, @array); # Loops and array syntax to be included in parallel sections
  
  ASSIGN: for my $assign (@assign)
    {
      my @p = &F ('ancestor::*', $assign);
  
      for my $p (@p)
        {
          # This assignment statement belongs to a parallel section set by the user; skip
          next ASSIGN if ($p->nodeName eq 'parallel-section');
        }
  
      for my $p (@p)
        {
          next unless ($p->nodeName eq 'do-construct');
          my ($v) = &F ('./do-stmt/do-V', $p, 1);
          if ($it{$v}) # This loop can be parallelized
            {
              $do{$p->unique_key} = $p;
              next ASSIGN;
            }
        }
  
      # No loop was found, this is array syntax
  
      push @array, $assign;
    }
  
  # Private routine to merge two parallel sections (if they are siblings)

  my $mergeParallel = sub
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
  };
  
  # Create a parallel section for each loop, and for each line of array syntax

  for my $do (values (%do), @array)
    {
      my $par = &n ('<parallel-section/>');
      my $p = $do->parentNode;
      $p->replaceChild ($par, $do);
      $par->appendChild ($_) for ($do, &t ("\n"));
    }
  
  my @par = &F ('.//parallel-section', $ep);
  
  PAR : for (my $i = 0; $i < scalar (@par)-1; $i++)
    {
      my @stmt0 = &F ('.//ANY-stmt', $par[$i+0]);
      my @stmt1 = &F ('.//ANY-stmt', $par[$i+1]);

      for (@stmt0, @stmt1)
        {
          next PAR if ($_->nodeName eq 'call-stmt');
        }

      my $count0 = scalar (@stmt0);
      my $count1 = scalar (@stmt1);

      next if ($count0 + $count1 > $opts{'max-statements-per-parallel'});

      if ($mergeParallel->($par[$i+0], $par[$i+1]))
        {
          splice (@par, $i+1, 1); 
          $i--; 
        }
    }
  
}

1;
