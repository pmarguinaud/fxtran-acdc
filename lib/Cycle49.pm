package Cycle49;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Construct;
use Data::Dumper;

sub simplify
{
  my $d = shift;
  my %opts = @_;

  &Construct::changeIfStatementsInIfConstructs ($d);

  my $zero = &e ('1');
  $zero->firstChild->firstChild->replaceNode (&t ('0'));

  &Construct::apply 
  ($d, 
    '//named-E[string(N)="LMUSCLFA"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDSPP_CONFIG%LSPP"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="OCH1CONV"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]',                      => &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]'     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_U"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_V"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_TH"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_SV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="LLSIMPLE_DGEMM"]'                        => &e ('.TRUE.'),
    '//named-E[string(.)="LLVERINT_ON_CPU"]'                       => &e ('.FALSE.'),
  );

  if (my $set = $opts{set})
    {
      my %set;

      for my $kv (@$set)
        {
          my ($k, $v) = split (m/=/o, $kv);
          $k = '//named-E[string(.)="' . $k . '"]';
          $v = $v eq '0' ? $zero : &e ($v);
          $set{$k} = $v;
        }
      
      &Construct::apply ($d, %set);

    }

}

1;
