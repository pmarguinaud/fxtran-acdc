#!/bin/bash

set -e

mkdir -p /tmp/$USER
export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/bin:$PATH

no_alloc=FIELD_2IM,FIELD_2LM,FIELD_2RB,FIELD_2RD,FIELD_2RM,FIELD_3IM,FIELD_3LM,FIELD_3RB,FIELD_3RD,FIELD_3RM,FIELD_4IM,FIELD_4LM,FIELD_4RB,FIELD_4RD,FIELD_4RM,FIELD_5IM,FIELD_5LM,FIELD_5RB,FIELD_5RD,FIELD_5RM

module_map=""

for k in "RM" "RB" "IM" "LM" "RD"
do
for i in 2 3 4 5
do
for e in "" "_PTR" "_VIEW"
do
  m="UTIL_FIELD_${i}${k}${e}_MOD,FIELD_UTIL_MODULE,UTIL_FIELD_${i}${k}${e}_ARRAY_MOD,FIELD_ARRAY_UTIL_MODULE"
  if [ "x$module_map" = "x" ]
  then
    module_map="$m"
  else
    module_map="$module_map,$m"
  fi
done
done
done

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

set -x

dir=src/local/ifsaux/util

# -wipe --copy --load --save

generateStructureMethods.pl \
  --host --dir $dir --tmp /tmp/$USER \
  $(resolve arpifs/module/type_fluxes.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/ptrxfu_type.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/ptrgfu_type.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/yomxfu_type.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/yomcfu_type.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_var --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_var --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/variable_module.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_cpg --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_type_mod.F90)

generateStructureMethods.pl \
  --host --dir $dir \
  --field-api --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/field_variables_mod.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components info_cpg --no-allocate $no_alloc --tmp /tmp/$USER \
  --module-map $module_map --field-api --field-api-class info_cpg \
  $(resolve .fypp/arpifs/module/mf_phys_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --load --save --dir $dir --tmp /tmp/$USER \
  $(resolve arpifs/module/yomcli.F90)

generateStructureMethods.pl \
  --host --wipe --copy --load --save --dir $dir --tmp /tmp/$USER --dir $dir \
  $(resolve arpifs/module/surface_fields_mix.F90)

generateStructureMethods.pl \
  --host --dir $dir --field-api --field-api-class info_sfc --tmp /tmp/$USER \
  --module-map $module_map $(resolve .fypp/arpifs/module/surface_variables_mod.F90)

generateStructureMethods.pl \
  --host \
  --dir $dir \
  --module-map $module_map --no-allocate $no_alloc \
  --skip-components info_sfv --field-api --field-api-class info_sfv --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/surface_views_prognostic_module.F90)

generateStructureMethods.pl \
  --host --dir $dir \
  --module-map $module_map --no-allocate $no_alloc --tmp /tmp/$USER \
  --skip-components info_sfv --field-api --field-api-class info_sfv \
  $(resolve .fypp/arpifs/module/surface_views_diagnostic_module.F90)

generateStructureMethods.pl \
  --host --dir $dir \
  --module-map $module_map --no-allocate $no_alloc --field-api --field-api-class info_sfc --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/mf_phys_surface_type_mod.F90)
  
generateStructureMethods.pl \
  --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_base_state_type_mod.F90)

generateStructureMethods.pl \
  --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_next_state_type_mod.F90)

linkTypes.pl
