#!/bin/bash

set -e
set -x

. $(dirname $0)/prolog.sh

dir=src/local/ifsaux/util

for f in hub/main/build/field_api/field_???_array_module.F90
do
generateStructureMethods.pl \
  --host --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $f
done

fieldRB.pl

generateStructureMethods.pl \
  --crc64 --legacy --size --wipe --copy --host --dir $dir --skip-components var  \
  --field-api --field-api-class var --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/variable_module.F90)

generateStructureMethods.pl \
  --crc64 --legacy --size --wipe --copy --host --dir $dir \
  --field-api --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/field_variables_mod.F90)

generateStructureMethods.pl \
  --size --wipe --copy --host --dir $dir --tmp /tmp/$USER \
  $(resolve arpifs/module/type_fluxes.F90)

generateStructureMethods.pl \
  --size --wipe --copy --host --dir $dir --skip-components flu  \
  --field-api --field-api-class flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/ptrxfu_type.F90)

generateStructureMethods.pl \
  --size --wipe --copy --host --dir $dir --skip-components flu  \
  --field-api --field-api-class flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/ptrgfu_type.F90)

generateStructureMethods.pl \
  --size --wipe --copy --host --dir $dir --skip-components TXFU%XFUBUF_B,TXFU%RMWINDCALC_B,TXFU%RMNWINDCALC_B  \
  --field-api --field-api-class flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/yomxfu_type.F90)

generateStructureMethods.pl \
  --size --wipe --copy --host --dir $dir --skip-components TCFU%CFUBUF_B  \
  --field-api --field-api-class flu --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/yomcfu_type.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_ddh_tnd_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve arpifs/module/cpg_sl_mask_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_gmv_type_mod.F90)

generateStructureMethods.pl \
  --host --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_gfl_type_mod.F90)


generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve arpifs/module/cpg_slmisc_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_sl1_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_sl2_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  \
  --field-api --field-api-class cpg --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/yomdgradient_type_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --skip-components cpg  --tmp /tmp/$USER \
  --field-api --field-api-class cpg \
  $(resolve .fypp/arpifs/module/mf_phys_type_mod.F90)

generateStructureMethods.pl \
  --crc64 --host --wipe --copy --load --save --dir $dir --tmp /tmp/$USER \
  $(resolve arpifs/module/yomcli.F90)

generateStructureMethods.pl \
  --crc64 --host --wipe --copy --load --save --dir $dir --tmp /tmp/$USER --dir $dir \
  $(resolve arpifs/module/surface_fields_mix.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --field-api --field-api-class sfc --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/surface_variables_mod.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir \
  --skip-components sfv --field-api --field-api-class sfv --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/surface_views_prognostic_module.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir --tmp /tmp/$USER \
  --skip-components sfv --field-api --field-api-class sfv \
  $(resolve .fypp/arpifs/module/surface_views_diagnostic_module.F90)

generateStructureMethods.pl \
  --host --wipe --copy --dir $dir \
  --field-api --field-api-class sfc --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/mf_phys_surface_type_mod.F90)
  
generateStructureMethods.pl \
  --dir $dir --wipe --copy --tmp /tmp/$USER  --field-api --field-api-class cpg $(resolve .fypp/arpifs/module/mf_phys_base_state_type_mod.F90)

generateStructureMethods.pl \
  --dir $dir --wipe --copy --tmp /tmp/$USER  --field-api --field-api-class cpg $(resolve .fypp/arpifs/module/mf_phys_next_state_type_mod.F90)


linkTypes.pl
