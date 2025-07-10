package Fxtran::Pointer::Parallel::OpenMPMethod;

#
# Copyright 2023 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use strict;

use Fxtran::Pointer::Parallel;
use Fxtran;

sub getAddBlockIndex
{
  1;
}

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
  my ($pu, $par1, $t, %opts) = @_;

  my $style = $opts{style};
  $style ||= 'Fxtran::Style'->new (style => ($par1->getAttribute ('style') || 'IAL'));
  
  my $FILTER = $par1->getAttribute ('filter');

  if ($FILTER)
    {
      die;
    }

  my ($do) = &F ('./do-construct', $par1);
  my $do_jblk = $do->firstChild;

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
      $do->parentNode->insertBefore (&s ("\n"), $do);

      if ($stmt->nodeName eq 'call-stmt')
        {
          my ($proc) = &F ('./procedure-designator/named-E', $stmt);
          my @R = &F ('./R-LT/ANY-R', $proc);
          next unless (my $comp = $R[-1]);

          if ($comp->nodeName eq 'component-R')
            {
              my ($ct) = &F ('./ct/text()', $comp);
              $ct->setData ($ct->data () . '_HOST_PARALLEL');
            }
          
        }
    }

  $do->unbindNode ();

  return $par1;
}


1;
