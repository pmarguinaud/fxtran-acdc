#!/bin/bash

set -e

dir=support
no_alloc=FIELD_2D,FIELD_3D,FIELD_4D,FIELD_5D,FIELD_INT2D,FIELD_INT3D,FIELD_INT4D,FIELD_INT5D,FIELD_LOG2D,FIELD_LOG3D,FIELD_LOG4D,FIELD_LOG5D
module_map=UTIL_FIELD_2D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_3D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_4D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_5D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_INT2D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_INT3D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_INT4D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_INT5D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_LOG2D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_LOG3D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_LOG4D_MOD,UTIL_FIELD_MOD,UTIL_FIELD_LOG5D_MOD,UTIL_FIELD_MOD



function resolve ()
{
  f=$1
  f=$(basename $f)
  if [ -f "support/$f" ]
  then
    echo "support/$f"
  else
    echo "tmp/$f"
  fi
}

set -x

export PATH=/opt/softs/anaconda3/bin:$PATH

mkdir -p tmp
rm -f tmp/*

for F90 in \
  .fypp/arpifs/module/mf_phys_surface_type_mod.F90  \
  .fypp/arpifs/module/surface_views_diagnostic_module.F90  \
  .fypp/arpifs/module/surface_views_prognostic_module.F90  \
  .fypp/arpifs/module/surface_variables_mod.F90  \
  .fypp/arpifs/module/cpg_opts_type_mod.F90  \
  .fypp/arpifs/module/mf_phys_type_mod.F90  \
  .fypp/arpifs/module/field_variables_mod.F90  \
  .fypp/arpifs/module/cpg_type_mod.F90  \
  .fypp/arpifs/module/variable_module.F90  \
  .fypp/arpifs/module/yomcfu_type.F90  \
  .fypp/arpifs/module/array_mod.F90  \
  .fypp/arpifs/module/yomxfu_type.F90
do
  b=$(basename $F90 .F90)
  if [ -f "support/$b.fypp" ]
  then
    fypp -m os -M support  -m yaml -m field_config support/$b.fypp tmp/$b.F90
  fi
done

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir \
  $(resolve arpifs/module/type_fluxes.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu \
  $(resolve .fypp/arpifs/module/yomxfu_type.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_flu --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_flu \
  $(resolve .fypp/arpifs/module/yomcfu_type.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_var --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_var \
  $(resolve .fypp/arpifs/module/variable_module.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_cpg --no-allocate $no_alloc \
  --only-types 'CPG_MISC_TYPE,CPG_DYN_TYPE,CPG_RCP_TYPE,CPG_CTY_TYPE,CPG_HWIND_TYPE,CPG_XYB_TYPE' \
  --module-map $module_map --field-api --field-api-class info_cpg \
  $(resolve .fypp/arpifs/module/cpg_type_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir \
  --only-components 'GEOMETRY_VARIABLES%GM,FIELD_VARIABLES%Q,FIELD_VARIABLES%O3,FIELD_VARIABLES%GEOMETRY' --only-types 'GEOMETRY_VARIABLES,FIELD_VARIABLES' \
  --field-api \
  $(resolve .fypp/arpifs/module/field_variables_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_cpg --no-allocate $no_alloc \
  --only-types MF_PHYS_OUT_TYPE \
  --module-map $module_map --field-api --field-api-class info_cpg \
  $(resolve .fypp/arpifs/module/mf_phys_type_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --out util_cpg_opts_type_mod.F90 \
  $(resolve .fypp/arpifs/module/cpg_opts_type_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir \
  $(resolve arpifs/module/yomcli.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir \
  --only-types TYPE_SURF_GEN --dir $dir \
  $(resolve arpifs/module/surface_fields_mix.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --out util_surface_variables_mod.F90 --field-api --field-api-class info_sfc \
  --only-types SURFACE_VARIABLE_GROUP_RESVR --skip-components SURFACE_VARIABLE_GROUP_RESVR%F_GROUP \
  $(resolve .fypp/arpifs/module/surface_variables_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --only-types SURFACE_VIEW_GROUP_RESVR,SURFACE_VIEW_GROUP_SNOWG,SURFACE_VIEW_GROUP_CLS \
  --dir $dir --out util_surface_views_prognostic_module.F90 \
  --module-map $module_map --no-allocate $no_alloc \
  --skip-components info_sfv --field-api --field-api-class info_sfv \
  $(resolve .fypp/arpifs/module/surface_views_prognostic_module.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --only-types SURFACE_VIEW_GROUP_VPRECIP,SURFACE_VIEW_GROUP_VPRECIP2 \
  --dir $dir --out util_surface_views_diagnostic_module.F90 \
  --module-map $module_map --no-allocate $no_alloc \
  --skip-components info_sfv --field-api --field-api-class info_sfv \
  $(resolve .fypp/arpifs/module/surface_views_diagnostic_module.F90)

./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir \
  --module-map $module_map --no-allocate $no_alloc --field-api --field-api-class info_sfc \
  --only-components 'MF_PHYS_SURF_TYPE%GSP_RR,MF_PHYS_SURF_TYPE%GSP_SG,MF_PHYS_SURF_TYPE%GSD_XP,MF_PHYS_SURF_TYPE%GSD_XP2,MF_PHYS_SURF_TYPE%GSP_CL' \
  $(resolve .fypp/arpifs/module/mf_phys_surface_type_mod.F90)
  
./scripts/loadsave.pl \
  --wipe --copy --load --save --dir $dir --skip-components info_arr --no-allocate $no_alloc \
  --module-map $module_map --field-api --field-api-class info_arr --out util_array_mod.F90 \
  $(resolve .fypp/arpifs/module/array_mod.F90)

./scripts/loadsave.pl \
  --wipe --copy --field-api --field-api-class info_sta \
  --dir $dir --no-allocate $no_alloc --module-map $module_map --field-api \
  $(resolve stack_mod.F90)
