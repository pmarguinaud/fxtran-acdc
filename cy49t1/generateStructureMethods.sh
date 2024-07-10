#!/bin/bash

set -x
set -e

. $(dirname $0)/prolog.sh

mkdir -p src/local/ifsaux/util

for f in \
phyex/aux/modd_misc.F90 \
phyex/aux/modd_phyex.F90 \
phyex/aux/modd_cst.F90 \
phyex/micro/modd_param_icen.F90 \
phyex/micro/modd_rain_ice_descrn.F90 \
phyex/micro/modd_rain_ice_paramn.F90 \
phyex/micro/modd_cloudparn.F90 \
phyex/turb/modd_param_mfshalln.F90 \
phyex/turb/modd_cturb.F90 \
phyex/turb/modd_turbn.F90 \
phyex/micro/modd_nebn.F90 \
phyex/micro/modd_param_lima.F90 \
phyex/micro/modd_param_lima_warm.F90 \
phyex/micro/modd_param_lima_cold.F90 \
phyex/micro/modd_param_lima_mixed.F90 \
phyex/aux/modd_nsv.F90 \
phyex/conv/modd_convpar.F90 \
phyex/conv/modd_convparext.F90 \
phyex/conv/modd_convpar_shal.F90 \
phyex/aux/modd_field.F90
do
  set -x
  generateStructureMethods.pl \
     --skip-components MISC_T%TBUCONF \
     --dir src/local/phyex/util --tmp /tmp/$USER \
     --crc64 --size --host --copy --wipe --save --load $(resolve $f)
  set +x
done

for f in \
arpifs/module/yompertpar.F90 \
arpifs/module/yoaiop.F90 \
arpifs/module/yomnrtaer.F90 \
arpifs/module/yomdgradient.F90 \
arpifs/module/yomnrtaer.F90 \
arpifs/module/yomdwet.F90 \
arpifs/module/yomcape.F90 \
arpifs/module/yoeozoc.F90 \
arpifs/module/cplng_types_mod.F90 \
arpifs/module/type_geometry.F90 \
arpifs/module/model_atmos_ocean_coupling_mod.F90 \
arpifs/module/model_chem_mod.F90 \
arpifs/module/model_diagnostics_mod.F90 \
arpifs/module/model_dynamics_mod.F90 \
arpifs/module/model_general_conf_mod.F90 \
arpifs/module/model_physics_aerosol_mod.F90 \
arpifs/module/model_physics_ecmwf_mod.F90 \
arpifs/module/model_physics_general_mod.F90 \
arpifs/module/model_physics_mf_mod.F90 \
arpifs/module/model_physics_radiation_mod.F90 \
arpifs/module/model_physics_simplinear_mod.F90 \
arpifs/module/model_physics_stochast_mod.F90 \
arpifs/module/type_model.F90 \
arpifs/module/yomprad.F90 \
arpifs/module/eint_mod.F90 \
arpifs/module/yomsphyhist.F90 \
arpifs/module/yomarphy.F90 \
arpifs/module/intdynsl_mod.F90 \
arpifs/module/yomcddh.F90 \
arpifs/module/yomchem.F90 \
arpifs/module/yomcompo.F90 \
arpifs/module/yomcom.F90 \
arpifs/module/yomcou.F90 \
arpifs/module/yomcsgeom.F90 \
arpifs/module/yomcsgeom.F90 \
arpifs/module/yomleg.F90 \
arpifs/module/yomcst.F90 \
arpifs/module/yomcumfs.F90 \
arpifs/module/yomcvmnh.F90 \
arpifs/module/yomdimf.F90 \
arpifs/module/yomdim.F90 \
arpifs/module/yomdimv.F90 \
arpifs/module/yomdphy.F90 \
arpifs/module/yomdprecips.F90 \
arpifs/module/yomdvisi.F90 \
arpifs/module/yomdyn.F90 \
arpifs/module/yomswe.F90 \
arpifs/module/yoeaeratm.F90 \
arpifs/module/yoeaerd.F90 \
arpifs/module/yoeaerlid.F90 \
arpifs/module/yoeaermap.F90 \
arpifs/module/yoeaersnk.F90 \
arpifs/module/yoeaersrc.F90 \
arpifs/module/yoeaervol.F90 \
arpifs/module/yoecldp.F90 \
arpifs/module/yoecld.F90 \
arpifs/module/yoecnd.F90 \
arpifs/module/yoe_cuconvca.F90 \
arpifs/module/yoecumf2.F90 \
arpifs/module/yoecumf.F90 \
arpifs/module/yoedbug.F90 \
arpifs/module/yemdim.F90 \
arpifs/module/yemdyn.F90 \
arpifs/module/yemgeo.F90 \
arpifs/module/yemgsl.F90 \
arpifs/module/yoegwd.F90 \
arpifs/module/yoegwdwms.F90 \
arpifs/module/yoegwwms.F90 \
arpifs/module/yemlbc_geo.F90 \
arpifs/module/yemlbc_model.F90 \
arpifs/module/yoelwrad.F90 \
arpifs/module/yoe_mcica.F90 \
arpifs/module/yemmp.F90 \
arpifs/module/yoeneur.F90 \
arpifs/module/yoeovlp.F90 \
arpifs/module/yoephli.F90 \
arpifs/module/yoephy.F90 \
arpifs/module/yoerad.F90 \
arpifs/module/yoerdi.F90 \
arpifs/module/yoerip.F90 \
arpifs/module/yoe_uvrad.F90 \
arpifs/module/yoewcou.F90 \
arpifs/module/yomgem.F90 \
arpifs/module/yomgpddh.F90 \
arpifs/module/yomgsgeom.F90 \
arpifs/module/yomgsgeom.F90 \
arpifs/module/yomhslmer.F90 \
arpifs/module/yomlap.F90 \
arpifs/module/yomlddh.F90 \
arpifs/module/yemlap.F90 \
arpifs/module/yomlouis.F90 \
arpifs/module/intdynsl_mod.F90 \
arpifs/module/yommcc.F90 \
arpifs/module/yommddh.F90 \
arpifs/module/yommp.F90 \
arpifs/module/yommse.F90 \
arpifs/module/yomncl.F90 \
arpifs/module/yomnorgwd.F90 \
arpifs/module/yomorog.F90 \
arpifs/module/yomorog.F90 \
arpifs/module/yomozo.F90 \
arpifs/module/yompaddh.F90 \
arpifs/module/yomparar.F90 \
arpifs/module/yophlc.F90 \
arpifs/module/yophnc.F90 \
arpifs/module/yomphy0.F90 \
arpifs/module/yomphy1.F90 \
arpifs/module/yomphy2.F90 \
arpifs/module/yomphy3.F90 \
arpifs/module/yomphyds.F90 \
arpifs/module/yomphy.F90 \
arpifs/module/ptrgppc.F90 \
arpifs/module/ptrslb15.F90 \
arpifs/module/ptrslb1.F90 \
arpifs/module/ptrslb2.F90 \
arpifs/module/yomradf.F90 \
arpifs/module/radiation_setup.F90 \
arpifs/module/yomrcoef.F90 \
arpifs/module/yomrip.F90 \
arpifs/module/intdynsl_mod.F90 \
arpifs/module/intdynsl_mod.F90 \
arpifs/module/yomsddh.F90 \
arpifs/module/yomsimphl.F90 \
arpifs/module/yomslphy.F90 \
arpifs/module/yomslrep.F90 \
arpifs/module/yomspddh.F90 \
arpifs/module/yoe_spectral_planck.F90 \
arpifs/module/type_spgeom.F90 \
arpifs/module/spng_mod.F90 \
arpifs/module/yomsrftlad.F90 \
arpifs/module/yomsta.F90 \
arpifs/module/stoph_mix.F90 \
arpifs/module/yomtddh.F90 \
arpifs/module/yoethf.F90 \
arpifs/module/yomtnh.F90 \
arpifs/module/yomtoph.F90 \
arpifs/module/yomtrc.F90 \
arpifs/module/yoevdf.F90 \
arpifs/module/yomvdoz.F90 \
arpifs/module/yomvert.F90 \
arpifs/module/yomcver.F90 \
arpifs/module/reglatlon_field_mix.F90 \
arpifs/module/yomvsleta.F90 \
arpifs/module/yomvsplip.F90 \
arpifs/module/yoeaeratm.F90 \
arpifs/module/yomdyna.F90 \
arpifs/module/yoeaerc.F90 \
arpifs/module/yoecmip.F90 \
arpifs/module/yoeradghg.F90 \
arpifs/module/yomslint.F90 \
arpifs/module/yoesw.F90 \
arpifs/module/type_ecv.F90 \
arpifs/module/yommoderrmod.F90 \
arpifs/module/spp_gen_mod.F90 \
arpifs/module/spp_def_mod.F90 
do
  set -x
  generateStructureMethods.pl \
     --skip-components MODEL_PHYSICS_RADIATION_TYPE%YRRADF,TYPE_GFL_COMP%PREVIOUS,MODEL_PHYSICS_STOCHAST_TYPE%YR_RANDOM_STREAMS,TEPHY%YSURF,TRADIATION%RAD_CONFIG,TECUCONVCA%YD_RANDOM_STREAM_CA,GEOMETRY%YRCSGEOMAD,GEOMETRY%YRGSGEOMAD,GEOMETRY%YROROG,GEOMETRY%YRCSGEOMAD_NB,GEOMETRY%YRGSGEOMAD_NB,MODEL%YRML_SPP,MODEL%YRML_SPPT,GEOMETRY%YRCSGEOM_NB,GEOMETRY%YRCSGEOM,GEOMETRY%YRCSGEOM_B,GEOMETRY%YRGSGEOM_NB,GEOMETRY%YRGSGEOM,GEOMETRY%YRGSGEOM_B,GEOMETRY%YRVETA,GEOMETRY%LNONHYD_GEOM,GEOMETRY%YRVAB,GEOMETRY%YRVFE,GEOMETRY%YRCVER,SL_STRUCT%MASK_SL1,SL_STRUCT%MASK_SL2,SL_STRUCT%MASK_SL2T,SL_STRUCT%MASK_SLD,SL_STRUCT%MASK_SLTOT  \
     --dir src/local/ifsaux/util --tmp /tmp/$USER \
     --crc64 --size --host --copy --wipe --save --load $(resolve $f)
  set +x
done


for f in \
arpifs/module/yoe_aerodiag.F90 \
arpifs/module/yom_ygfl.F90 
do
  set -x
  generateStructureMethods.pl \
     --crc64 --legacy --skip-components TYPE_GFL_COMP%PREVIOUS,MODEL_PHYSICS_STOCHAST_TYPE%YR_RANDOM_STREAMS,TEPHY%YSURF,TRADIATION%RAD_CONFIG,TECUCONVCA%YD_RANDOM_STREAM_CA,GEOMETRY%YRCSGEOMAD,GEOMETRY%YRGSGEOMAD,GEOMETRY%YROROG,GEOMETRY%YRCSGEOMAD_NB,GEOMETRY%YRGSGEOMAD_NB,MODEL%YRML_SPP,MODEL%YRML_SPPT,GEOMETRY%YRCSGEOM_NB,GEOMETRY%YRCSGEOM,GEOMETRY%YRCSGEOM_B,GEOMETRY%YRGSGEOM_NB,GEOMETRY%YRGSGEOM,GEOMETRY%YRGSGEOM_B,GEOMETRY%YRVETA,GEOMETRY%LNONHYD_GEOM,GEOMETRY%YRVAB,GEOMETRY%YRVFE,GEOMETRY%YRCVER,SL_STRUCT%MASK_SL1,SL_STRUCT%MASK_SL2,SL_STRUCT%MASK_SL2T,SL_STRUCT%MASK_SLD,SL_STRUCT%MASK_SLTOT  \
     --dir src/local/ifsaux/util --tmp /tmp/$USER \
     --size --host --copy --wipe --save --load $(resolve $f)
  set +x
done


set -x

#generateStructureMethods.pl \
#  --wipe --copy --load --save --dir src/local/ifsaux/util --out util_cpg_opts_type_mod.F90 \
#  $(resolve .fypp/arpifs/module/cpg_opts_type_mod.F90)


generateStructureMethods.pl \
  --crc64 --size --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  --only-types TSPP_CONFIG \
  $(resolve arpifs/module/spp_mod.F90)

generateStructureMethods.pl \
  --crc64 --size --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  --only-types TSPPT_CONFIG \
  $(resolve arpifs/module/yomspsdt.F90)

generateStructureMethods.pl \
  --crc64 --size --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  --only-types TGFLT,TGMVT,TTND,THWIND,TCTY,TRCP,TXYBDER,TXYB \
  $(resolve arpifs/module/intdyn_mod.F90)


generateStructureMethods.pl \
  --crc64 --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  $(resolve .fypp/arpifs/module/cpg_opts_type_mod.F90)

generateStructureMethods.pl \
  --crc64 --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  --only-types TYPE_SURF_GEN \
  $(resolve arpifs/module/surface_fields_mix.F90)

generateStructureMethods.pl \
  --crc64 --host --wipe --copy --load --save --dir src/local/ifsaux/util --tmp /tmp/$USER \
  $(resolve arpifs/module/yomcli.F90)

