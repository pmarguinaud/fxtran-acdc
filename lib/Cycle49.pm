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
 
  my @arpege;

  if ($opts{arpege})
    {
      my $zero = &e ('1');
      $zero->firstChild->firstChild->replaceNode (&t ('0'));
      @arpege =
      (
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%NCURRENT_ITER"]'  => $zero,
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%RINTOPT"]'        => &e ('1._JPRB'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%NSPLTHOI"]'       => &e ('0'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LADVF"]'          => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LRHS_CURV"]'      => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LSPLTHOIGFL"]'    => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHDYN"]'        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LGWADV"]'        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LPC_FULL"]'      => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LPC_CHEAP"]'     => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LMIXETTLS"]'     => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LCOMAD"]'        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSLHD"]'         => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_PHY_SLIN%YRPHLC%LSPHLC"]'   => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LTWOTL"]'        => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHEE"]'         => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHQE"]'         => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LELTRA"]'        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSVTSM"]'        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LVERTFE"]'       => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSPRT"]'         => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%NVDVAR"]'        => &e ('3'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%ND4SYS"]'        => &e ('2'),
        '//named-E[string(.)="LLCT"]'                                  => &e ('.FALSE.'),
        '//named-E[string(.)="LLCTC"]'                                 => &e ('.FALSE.'),
      );
    }

  &Construct::apply 
  ($d, 
    '//named-E[string(N)="LMUSCLFA"]',                          &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]',                   &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]', &e ('.FALSE.'),
    '//named-E[string(.)="YDSPP_CONFIG%LSPP"]',                 &e ('.FALSE.'),
    '//named-E[string(.)="LMCAPEA"]',                           &e ('.FALSE.'),
    '//named-E[string(.)="OCH1CONV"]',                          &e ('.FALSE.'),
    @arpege,
  );


}

1;
