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
 

  # Do not use inner pointers of YDGEOMETRY : please fix the code
  #  
  # The following pointers of YDGEOMETRY :
  #     TYPE(TVAB),POINTER     :: YRVAB => NULL()    
  #     TYPE(TVETA),POINTER    :: YRVETA => NULL()
  #     TYPE(TVFE),POINTER     :: YRVFE => NULL()
  #     TYPE(TCVER),POINTER    :: YRCVER => NULL()
  #     LOGICAL,POINTER  :: LNONHYD_GEOM => NULL()  
  # Point to the inner members of this component :
  #     TYPE(TVERTICAL_GEOM) :: YRVERT_GEOM

  
  my $die = 0;

  for my $m (qw (LNONHYD_GEOM YRVAB YRVETA YRVFE YRCVER))
    {
      my @expr = &F ('.//named-E[string(./N)="YDGEOMETRY"][./R-LT/component-R[string(./ct)="?"]]', $m, $d);
      for my $expr (@expr)
        {
          my @ct = &F ('./R-LT/component-R/ct/text()', $expr, 1);

          if ($ct[0] eq $m)
            {
              print $expr->textContent, "\n"; $die++;
             
              my ($rlt) = &F ('./R-LT', $expr);
#             $rlt->insertBefore (&n ('<component-R>%<ct>YRVERT_GEOM</ct></component-R>'), $rlt->firstChild);
            }

        }
    }

  die "Fix the code !" if ($die);

  my @arpege;

  my $zero = &e ('1');
  $zero->firstChild->firstChild->replaceNode (&t ('0'));

  if ($opts{arpege})
    {
      @arpege =
      (
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%NCURRENT_ITER"]'          => $zero,
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%RINTOPT"]'                => &e ('1._JPRB'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%NSPLTHOI"]'               => $zero,
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LADVF"]'                  => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LRHS_CURV"]'              => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYN%LSPLTHOIGFL"]'            => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSLDIA"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHDYN"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LGWADV"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LPC_FULL"]'              => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LPC_CHEAP"]'             => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LMIXETTLS"]'             => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LCOMAD"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSLHD"]'                 => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_PHY_SLIN%YRPHLC%LSPHLC"]'           => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LTWOTL"]'                => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHEE"]'                 => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LNHQE"]'                 => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LELTRA"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSVTSM"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDGEOMETRY%YRCVER%LVERTFE"]'                     => &e ('.TRUE.'),
        '//named-E[string(.)="YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE"]'         => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSPRT"]'                 => &e ('.TRUE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%NVDVAR"]'                => &e ('3'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%ND4SYS"]'                => &e ('2'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSLINL"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSACC"]'                 => &e ('.FALSE.'),
        '//named-E[string(.)="LLCT"]'                                          => &e ('.FALSE.'),
        '//named-E[string(.)="LLCTC"]'                                         => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_PHY_EC%YREPHY%LSLPHY"]'             => &e ('.FALSE.'),
        '//named-E[string(.)="YDCPG_OPTS%LSFORC"]'                             => &e ('.FALSE.'),
        '//named-E[string(.)="LGWDIAGS_ON"]'                                   => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRSPNG%LNSPONGE"]'              => &e ('.FALSE.'),
        '//named-E[string(.)="YDCPG_OPTS%LNUDG"]'                              => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_LBC%LTENC"]'                        => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LRSLDDH"]'              => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LSDDH"]'                => &e ('.FALSE.'),
        '//named-E[string(.)="YDCPG_OPTS%LSLPHY"]'                             => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_PHY_EC%YRECUCONVCA%LCUCONV_CA"]'    => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_PHY_MF%YRSIMPHL%LTRAJPS"]'          => &e ('.FALSE.'),
        '//named-E[string(.)="LTRAJSAVE"]'                                     => &e ('.FALSE.'),
        '//named-E[string(.)="YDMODEL%YRML_DYN%YRDYNA%LSLAG"]'                 => &e ('.TRUE.'),
      );

      my @tmp = @arpege;

      while (my ($k, $v) = splice (@tmp, 0, 2))
        {
          $k =~ s/YDMODEL%YRML_DYN/YDML_DYN/o;
          push @arpege, ($k, $v->cloneNode (1));
          $k =~ s/YDML_DYN%YRDYN/YDDYN/o;
          push @arpege, ($k, $v->cloneNode (1));
        }

    }

  &Construct::apply 
  ($d, 
    '//named-E[string(.)="N_VMASS"]'                               => $zero,
    '//named-E[string(N)="LMUSCLFA"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="YDLDDH%LFLEXDIA"]'                       => &e ('.FALSE.'),
    '//named-E[string(.)="YDMODEL%YRML_DIAG%YRLDDH%LFLEXDIA"]'     => &e ('.FALSE.'),
    '//named-E[string(.)="YDSPP_CONFIG%LSPP"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="LMCAPEA"]'                               => &e ('.FALSE.'),
    '//named-E[string(.)="OCH1CONV"]'                              => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_U"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_V"]'                      => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_TH"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_RV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="BUCONF%LBUDGET_SV"]'                     => &e ('.FALSE.'),
    '//named-E[string(.)="LLSIMPLE_DGEMM"]'                        => &e ('.TRUE.'),
    '//named-E[string(.)="LLVERINT_ON_CPU"]'                       => &e ('.FALSE.'),
    @arpege,
  );


}

1;
