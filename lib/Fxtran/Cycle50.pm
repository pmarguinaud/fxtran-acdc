package Fxtran::Cycle50;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Fxtran::Construct;
use Data::Dumper;

sub simplify
{
  shift;
  my $d = shift;
  my %opts = @_;

  my $zero = &e ('1');
  $zero->firstChild->firstChild->replaceNode (&t ('0'));

  &Fxtran::Construct::apply 
  ($d, 
    '//named-E[string(N)="OCND2"]'                                 => &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_PHY_FORCING%LMUSCLFA"]'     => &e ('.FALSE.'),
    '//named-E[string(.)="YDML_PHY_FORCING%LMUSCLFA"]'             => &e ('.FALSE.'),
    '//named-E[string(N)="LMUSCLFA"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDSPP_CONFIG%LSPP"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="OCH1CONV"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]',                      => &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]'     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RC"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RI"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_U"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_V"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_TH"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_SV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="LLSIMPLE_DGEMM"]'                        => &e ('.TRUE.'),
    '//named-E[string(.)="LLVERINT_ON_CPU"]'                       => &e ('.FALSE.'),
  );

  for my $n (&F ('.//n/text()[string(.)="YDGEO"', $d))
    {
      $n->setData ('YDGEOMETRY');
    }

  if (my $set = $opts{'set-variables'})
    {
      my %set;

      while (my ($k, $v) = each (%$set))
        {
          $k = '//named-E[string(.)="' . $k . '"]';
          $v = $v eq '0' ? $zero : &e ($v);
          $set{$k} = $v;
        }
      
      &Fxtran::Construct::apply ($d, %set);

    }

}

1;
