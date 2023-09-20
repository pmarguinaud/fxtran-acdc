package Pointer::Parallel::OpenACCMethod;

#
# Copyright 2023 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Pointer::Parallel;
use Fxtran;
use Data::Dumper;

sub getDefaultWhere
{
  'host';
}

sub onlySimpleFields
{
  1;
}

sub requireUtilMod
{
  0;
}

sub makeParallel
{
  shift;
  my ($par1, $t) = @_;

  my $style = $par1->getAttribute ('style') || 'ARPIFS';
  my $FILTER = $par1->getAttribute ('filter');

  if ($FILTER)
    {
      die;
    }

  my ($do) = &F ('./do-construct', $par1);
  my $do_jblk = $do->firstChild;
  my $indent = &Fxtran::getIndent ($do_jblk);

  my @stmt = &F ('./ANY-stmt', $do);
  shift (@stmt) for (1 .. 2);
  pop (@stmt);
  
  my %update;

  for my $expr (&F ('.//named-E', $par1))
    {
      my ($N) = &F ('./N', $expr, 1);
      my $s = $t->{$N};
      next if ($s->{skip});

      if ($s->{object})
        {
          $update{$N}++;
        }
    }

  for my $stmt (@stmt)
    {
      $do->parentNode->insertBefore ($stmt, $do);
      $do->parentNode->insertBefore (&s ((' ' x $indent) . "\n"), $do);

      if ($stmt->nodeName eq 'call-stmt')
        {
          my ($proc) = &F ('./procedure-designator/named-E', $stmt);
          my @R = &F ('./R-LT/ANY-R', $proc);
          next unless (my $comp = $R[-1]);

          if ($comp->nodeName eq 'component-R')
            {
              my ($ct) = &F ('./ct/text()', $comp);
              $ct->setData ($ct->data () . '_DEVICE_PARALLEL');
            }
          
        }
    }

  $do->unbindNode ();

  return $par1;
}


1;
