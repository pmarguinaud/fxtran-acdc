package Fxtran::Pointer::Parallel::BasicManyBlocks;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use strict;

use Fxtran::Pointer::Parallel;
use Fxtran;
use Fxtran::Message;

sub makeParallel
{
  shift;
  my ($par1, $t, %opts) = @_;

  for my $stmt (&F ('.//ANY-stmt', $par1))
    {
      if ($stmt->nodeName eq 'a-stmt')
        {
          my %blocked;

          for my $N (&F ('.//named-E/N', $stmt, 1))
            {
              my $s = $t->{$N};
              $blocked{$N}++ if ($s->{blocked});
            }

          &Fxtran::Message::error 
          (
             "NPROMA blocked variables:\n\n" 
           . join ("\n", map { " - $_\n" } sort keys (%blocked)) . "\n"
           . "are used following statement is forbidden in a ManyBlocks section",
           $stmt
          ) if (%blocked);

        }
    }

  my $style = $par1->getAttribute ('style') || 'IAL';
  $style = 'Fxtran::Style'->new (style => $style);

  my ($comp) = &F ('./comp', $par1);
  
  my $FILTER = $par1->getAttribute ('filter');

  if (my $it = $style->customIterator ())
    {
      my $it1 = $style->customIteratorCopy ();
      my @D = &F ('.//named-E[string(N)="?"]/N/n/text()', $it, $par1);
      for my $D (@D)
        {
          $D->setData ($it1);
        }
    }

  my ($KLON, $KGPTOT, $KGPBLKS, $JBLKMIN);

  if ($FILTER)
    {
      $KLON    = 'YL_FGS%KLON';
      $KGPTOT  = 'YL_FGS%KGPTOT';
      $KGPBLKS = 'YL_FGS%KGPBLKS';
      $JBLKMIN = '1';
    }
  else
    {
      $KLON    = 'YDCPG_OPTS%KLON';
      $KGPTOT  = 'YDCPG_OPTS%KGPCOMP';
      $KGPBLKS = 'YDCPG_OPTS%KGPBLKS';
      $JBLKMIN = 'YDCPG_OPTS%JBLKMIN';
    }

  my $LDACC = $opts{acc} ? &e ('.TRUE.') : &e ('.FALSE.');
  my $YDOFFSET = &e ('STACK (0, 0, 0, 0)');

  for my $call (&F ('.//call-stmt', $comp))
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);

      next if ($proc eq 'ABOR1');

      $proc->setData ($proc->textContent . $opts{'suffix-manyblocks'});
      my ($argspec) = &F ('./arg-spec', $call);

      $argspec->appendChild (&t (','));
      $argspec->appendChild (&n ('<arg><arg-N><k>LDACC</k></arg-N>=' . $LDACC . '</arg>'));

      if ($opts{'use-stack-manyblocks'})
        {
          $argspec->appendChild (&t (','));
          $argspec->appendChild (&n ('<arg><arg-N><k>YDOFFSET</k></arg-N>=' . $YDOFFSET . '</arg>'));
        }

      for my $arg (&F ('./arg/ANY-E', $argspec))
        {
          if ($arg->textContent eq 'YDCPG_OPTS%KLON')
            {
              $argspec->insertAfter ($_, $arg->parentNode) 
                for (&n ('<arg>' . &e ($KGPBLKS) . '</arg>'), &t (', '));
              $arg->replaceNode (&e ($KLON));
            }
          elsif ($arg->textContent eq 'YDCPG_BNDS%KFDIA')
            {
              my $kfdia = "MIN ($KLON, $KGPTOT - ($KGPBLKS - 1) * $KLON)";
              $arg->replaceNode (&e ($kfdia));
            }
        }

    }

  return $par1;
}

1;
