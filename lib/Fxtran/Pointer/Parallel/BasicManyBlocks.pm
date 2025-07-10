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
use Fxtran::ManyBlocks;

sub makeParallel
{
  shift;
  my ($pu, $par1, $t, %opts) = @_;

  my $style = $opts{style};

  my @nproma = $style->nproma (); my %nproma = map { ($_, 1) } @nproma;

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

  my (%var2dim, %typearg);

  while (my ($n, $s) = each (%$t))
    {
      if ($s->{isFieldAPI})
        {
          $var2dim{$n} = $s->{nd};
        }
      elsif ($s->{constant} && $s->{arg})
        {
          $typearg{$n} = 1;
        }
    }

  my $LDACC = $opts{acc} ? '.TRUE.' : '.FALSE.';

  my %dims = (kgpblks => 'YDCPG_OPTS%KGPBLKS', nproma => 'YDCPG_OPTS%KLON', kidia => '1',
              kfdia => "MIN ($KLON, $KGPTOT - ($KGPBLKS - 1) * $KLON)", jlon => $style->jlon (),
              kidia_call => 'YDCPG_BNDS%KIDIA', kfdia_call => 'YDCPG_BNDS%KFDIA');
  
  my @par = &F ('.//parallel-section', $par1);

  my %present;

  for my $par (@par)
    {
      $par = &Fxtran::ManyBlocks::processSingleSection ($pu, $par, \%var2dim, \%typearg, \%dims, $LDACC, %opts);

      for my $n (&F ('.//named-E/N', $par, 1))
        {
          $present{$n}++ if ($var2dim{$n} || $typearg{$n});
        }
    }

  my ($comp) = &F ('./comp', $par1);

  my $pragma = $opts{pragma};

  if ($opts{acc} && %present)
    {
      $pragma->insertData ($comp, PRESENT => [sort keys (%present)]);
    }

  my $LDACC = $opts{acc} ? &e ('.TRUE.') : &e ('.FALSE.');
  my $YDOFFSET = &e ('STACK (0, 0, 0, 0)');

  for my $call (&F ('.//call-stmt', $comp))
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);

      next if ($proc->textContent eq 'ABOR1');
      next if ($proc =~ m/$opts{'suffix-singlecolumn'}$/i);

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
          my $argt = $arg->textContent;
          my ($n) = &F ('./N', $arg, 1);

          if ($argt eq 'YDCPG_BNDS%KFDIA')
            {
              my $kfdia = "MIN ($KLON, $KGPTOT - ($KGPBLKS - 1) * $KLON)";
              $arg->replaceNode (&e ($kfdia));
            }
          elsif ($n && $var2dim{$n} && (my ($sslt) = &F ('.//array-R/section-subscript-LT', $arg)))
            {
              $sslt->appendChild ($_) for (&t (','), &n ('<section-subscript>:</section-subscript'));
            }
        }

      $argspec->appendChild ($_) for (&t (', '), &n ('<arg><arg-N><k>KGPBLKS</k></arg-N>=' . &e ($KGPBLKS) . '</arg>'));

    }

  return $par1;
}

1;
