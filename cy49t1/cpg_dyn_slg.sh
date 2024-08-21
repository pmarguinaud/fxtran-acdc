#!/bin/bash

set -x
set -e

. $(dirname $0)/prolog.sh

for f in \
  arpifs/adiab/cpglag.F90                                                \
  .fypp/arpifs/adiab/gptf1_ydvars.F90                                    \
  arpifs/adiab/gpmpfc_expl_part2.F90                                     \
  arpifs/adiab/gpinislb_part2_expl.F90                                   \
  arpifs/adiab/cpg_gp.F90                                                \
  arpifs/adiab/gpmpfc_expl.F90                                           \
  arpifs/adiab/cpg_gp_hyd.F90                                            \
  arpifs/adiab/lavent.F90                                                \
  arpifs/adiab/lacdyn.F90                                                \
  arpifs/adiab/cpg_dyn_slg.F90                                           \
  arpifs/adiab/lassie.F90                                                \
  arpifs/adiab/lattex.F90                                                \
  arpifs/adiab/lattex_expl_2tl.F90                                       \
  arpifs/adiab/lattex_expl_vspltrans.F90                                 \
  arpifs/adiab/lattes.F90                                                \
  arpifs/adiab/lavabo.F90                                                \
  arpifs/adiab/lavabo_expl_laitvspcqm_part1.F90                          \
  arpifs/adiab/lavabo_expl_laitvspcqm_part2.F90                          \
  .fypp/arpifs/adiab/gprcp_expl.F90                                      
do

dir=$(perl -e ' 
use File::Basename; 
my $f = shift; 
$f =~ s,^\.fypp/,,o; 
print &dirname ($f) 
' $f)

pointerParallel.pl \
  --gpumemstat --stack84 --jlon JROF --nproma YDCPG_OPTS%KLON,YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIM%NPROMNH --cycle 49 --use-acpy \
  --types-fieldapi-dir types-fieldapi --post-parallel synchost,nullify --version --dir \
  src/local/$dir $(resolve $f)

done

for f in \
  arpifs/adiab/lasure.F90                         \
  arpifs/adiab/lattex_dnt.F90                     \
  arpifs/adiab/lattex_tnt.F90                     \
  arpifs/adiab/gptf2_expl_2tl.F90                 \
  arpifs/utility/verdisint.F90                    \
  arpifs/adiab/gp_spv.F90                         \
  arpifs/adiab/gpinislb_part1_expl.F90            \
  arpifs/adiab/gpinislb_part3_expl.F90            \
  arpifs/adiab/gprt.F90                           \
  arpifs/adiab/gpcty_expl.F90                     \
  arpifs/adiab/gpgeo_expl.F90                     \
  arpifs/adiab/gpgrgeo_expl.F90                   \
  arpifs/adiab/gphlwi.F90                         \
  arpifs/adiab/gphluv_expl.F90                    \
  arpifs/adiab/gpuvs.F90                          \
  arpifs/adiab/gpgrp_expl.F90                     \
  arpifs/adiab/gpxx.F90                           \
  arpifs/adiab/gp_tndlagadiab_uv.F90              \
  arpifs/adiab/gphpre_expl_vertfe0.F90            \
  arpifs/adiab/gphpre_expl_vertfe1.F90            \
  arpifs/adiab/gphpre_expl.F90                    \
  arpifs/adiab/gpgrxyb_expl.F90                   \
  arpifs/adiab/gpgw.F90                           \
  arpifs/adiab/gpmpfc_expl_part1.F90            
do

dir=$(dirname $f)
openacc.pl --interface --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --cpg_dyn --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)

done

for f in \
  .fypp/arpifs/adiab/sitnu_gp.F90                 \
  .fypp/arpifs/adiab/sigam_gp.F90
do

dir=$(perl -e ' 
use File::Basename; 
my $f = shift; 
$f =~ s,^\.fypp/,,o; 
print &dirname ($f) 
' $f)

openacc.pl --interface --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)

done

for f in \
  arpifs/phys_dmn/dprecips_xfu.F90 \
  arpifs/dia/meanwind_xfu.F90
do

dir=$(dirname $f)
openacc.pl --interface --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)

done

