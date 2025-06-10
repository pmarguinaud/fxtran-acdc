#!/bin/bash

set -x
set -e

. $(dirname $0)/prolog.sh

for f in \
  arpifs/adiab/cputqy0.F90                                               \
  arpifs/adiab/cputqy_arome_expl.F90                                     \
  arpifs/phys_dmn/aro_turb_mnh.F90                                       \
  arpifs/phys_dmn/mf_phys_precips.F90                                    \
  arpifs/phys_dmn/mf_phys_save_phsurf_part1.F90                          \
  arpifs/phys_dmn/mf_phys_save_phsurf_part2.F90                          \
  arpifs/phys_dmn/mf_phys_transfer.F90                                   \
  arpifs/phys_dmn/apl_arome.F90                                          \
  arpifs/phys_dmn/apl_arome_surface.F90                                  \
  arpifs/phys_dmn/apl_arome_radiation.F90                                \
  arpifs/phys_dmn/aro_adjust.F90                                         \
  arpifs/phys_dmn/apl_arome_adjust.F90                                   \
  arpifs/phys_dmn/apl_arome_init.F90                                     \
  arpifs/phys_radi/recmwf.F90                                            \
  arpifs/phys_dmn/apl_arome_init.F90                                     \
  arpifs/phys_dmn/apl_arome_adjust.F90                                   \
  arpifs/phys_dmn/apl_arome_shallow.F90                                  \
  arpifs/phys_dmn/apl_arome_turbulence.F90                               \
  arpifs/phys_dmn/apl_arome_micro.F90                                    \
  arpifs/phys_dmn/apl_arome_nudge.F90                                    \
  arpifs/phys_dmn/apl_arome_final.F90                                    \
  arpifs/phys_dmn/apl_arome_tw_tendency.F90

do
# pointerParallel.pl --types-fieldapi-dir types-fieldapi --post-parallel synchost --only-if-newer --version src/local/$f 
  dir=$(dirname $f)
  pointerParallel.pl \
   --cycle 50 --gpumemstat --stack84 --use-acpy \
   --types-fieldapi-dir types-fieldapi --post-parallel synchost,nullify \
   --inline-contains --version --dir src/local/$dir $(resolve --user $f)
done

