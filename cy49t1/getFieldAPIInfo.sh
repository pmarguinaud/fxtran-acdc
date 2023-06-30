#!/bin/bash

export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/bin:$PATH

function resolve ()
{
  f=$1
  for view in $(cat .gmkview)
  do
    g="src/$view/$f"
    if [ -f $g ]
    then
      echo $g
      break
    fi
  done
}

generateStructureMethods.pl \
 --field-api --field-api-class info_var $(resolve .fypp/arpifs/module/variable_module.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_var $(resolve .fypp/arpifs/module/field_variables_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_type_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_base_state_type_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_next_state_type_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/cpg_type_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_sfv $(resolve .fypp/arpifs/module/surface_views_diagnostic_module.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_sfv $(resolve .fypp/arpifs/module/surface_views_prognostic_module.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_sfc $(resolve .fypp/arpifs/module/surface_variables_mod.F90)

generateStructureMethods.pl \
 --field-api --field-api-class info_sfc $(resolve .fypp/arpifs/module/mf_phys_surface_type_mod.F90)

linkTypes.pl


