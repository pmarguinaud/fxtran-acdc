#!/bin/bash

set -e
set -x

. $(dirname $0)/prolog.sh

tmp=/tmp/marguina

generateStructureMethods.pl \
 --tmp $tmp  --field-api --field-api-class info_var $(resolve .fypp/arpifs/module/variable_module.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_var $(resolve .fypp/arpifs/module/field_variables_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_base_state_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/mf_phys_next_state_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg $(resolve .fypp/arpifs/module/cpg_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_sfv $(resolve .fypp/arpifs/module/surface_views_diagnostic_module.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_sfv $(resolve .fypp/arpifs/module/surface_views_prognostic_module.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_sfc $(resolve .fypp/arpifs/module/surface_variables_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_sfc $(resolve .fypp/arpifs/module/mf_phys_surface_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg $(resolve arpifs/module/cpg_slmisc_type_mod.F90)

generateStructureMethods.pl \
  --tmp $tmp --field-api --field-api-class info_cpg  hub/main/build/field_api/field_array_module.F90

linkTypes.pl


