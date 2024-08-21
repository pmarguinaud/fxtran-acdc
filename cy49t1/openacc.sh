#!/bin/bash

set -e
set -x

. $(dirname $0)/prolog.sh

for f in \
   arpifs/phys_radi/acralu.F90                     \
   arpifs/phys_dmn/actke1.F90                      \
   arpifs/phys_dmn/apl_arpege_turbulence_part2.F90 \
   arpifs/phys_dmn/acprod.F90                      \
   arpifs/phys_dmn/actke2.F90                      \
   arpifs/phys_dmn/actke.F90                       \
   arpifs/phys_dmn/acbl89.F90                      \
   arpifs/phys_dmn/acturb.F90                      \
   arpifs/phys_dmn/acevolet.F90                    \
   arpifs/phys_dmn/aclender.F90                    \
   arpifs/phys_dmn/hl2fl.F90                       \
   arpifs/phys_dmn/fl2hl.F90                       \
   arpifs/phys_dmn/dprecips.F90                    \
   arpifs/pp_obs/ppwetpoint.F90                    \
   arpifs/phys_dmn/acdrag.F90                      \
   arpifs/phys_dmn/acpluis.F90                     \
   arpifs/phys_dmn/acdnshf.F90                     \
   arpifs/phys_radi/radozcmf.F90                   \
   arpifs/phys_dmn/suozon.F90                      \
   arpifs/adiab/cpphinp.F90                        \
   arpifs/phys_dmn/aplpar_init.F90                 \
   arpifs/phys_dmn/actqsat.F90                     \
   arpifs/phys_dmn/acsol.F90                       \
   arpifs/phys_dmn/acsolw.F90                      \
   arpifs/phys_dmn/achmt.F90                       \
   arpifs/phys_dmn/acntcls.F90                     \
   arpifs/phys_dmn/achmtls.F90                     \
   arpifs/phys_dmn/acclph.F90                      \
   arpifs/phys_dmn/qngcor.F90                      \
   arpifs/phys_dmn/acdrme.F90                      \
   arpifs/phys_dmn/acevadcape.F90                  \
   arpifs/phys_dmn/accldia.F90                     \
   arpifs/phys_dmn/acvisih.F90                     \
   arpifs/phys_dmn/mf_phys_precips.F90             \
   arpifs/phys_dmn/mf_phys_bayrad.F90              \
   arpifs/phys_dmn/acfluso.F90                     \
   arpifs/phys_radi/radaer.F90                     \
   arpifs/phys_dmn/acnebcond.F90                   \
   arpifs/phys_dmn/acnebsm.F90                     \
   arpifs/phys_dmn/acnebn.F90                      \
   arpifs/phys_dmn/acnebxrs.F90                    \
   arpifs/phys_dmn/acnpart.F90                     \
   arpifs/phys_dmn/acnpart_cloud_cover_wmo.F90     \
   arpifs/phys_dmn/acnpart_cloud_cover.F90         \
   arpifs/phys_dmn/acnpart_conv_base_top.F90       \
   arpifs/phys_radi/radheat.F90                    \
   arpifs/phys_dmn/acdayd.F90                      \
   arpifs/phys_radi/acrso.F90                      \
   arpifs/phys_dmn/acveg.F90                       \
   arpifs/phys_dmn/acdifv1.F90                     \
   arpifs/phys_dmn/arp_ground_param.F90            \
   arpifs/phys_dmn/acdifv2.F90                     \
   arpifs/phys_dmn/tridifv1.F90                    \
   arpifs/phys_dmn/acuptq.F90                      \
   arpifs/phys_dmn/acpluiz.F90                     \
   arpifs/phys_dmn/acmicro.F90                     \
   arpifs/phys_dmn/advprcs.F90                     \
   arpifs/phys_dmn/acdrov.F90                      \
   arpifs/phys_dmn/acdrov.F90                      \
   arpifs/adiab/cpwts.F90                          \
   arpifs/adiab/cptends.F90                        \
   arpifs/adiab/cptend_new.F90                     \
   arpifs/adiab/cpmvvps.F90                        
do 
  echo "==> $f <=="
  dir=$(dirname $f)
  mkdir -p src/local/ifsaux/openacc/$dir
#  --only-if-newer 
  openacc.pl --interface --stack84 \
   --dir src/local/ifsaux/openacc/$dir \
   --nocompute ABOR1 --version \
   $(resolve --user $f)
done


for f in \
   arpifs/phys_ec/cuentr.F90                       \
   arpifs/phys_ec/cubasmcn.F90                     \
   arpifs/phys_ec/cuadjtq.F90                      \
   arpifs/phys_ec/cuadjtqs.F90                     
do
  resolve --user $f 
done

for f in \
   arpifs/phys_ec/cuentr.F90                       \
   arpifs/phys_ec/cubasmcn.F90                     \
   arpifs/phys_ec/cuadjtq.F90                      \
   arpifs/phys_ec/cuadjtqs.F90                     \
   arpifs/phys_dmn/cucalln_mf.F90                  \
   arpifs/phys_ec/cuascn.F90                       \
   arpifs/phys_ec/cubasen.F90                      \
   arpifs/phys_ec/cuddrafn.F90                     \
   arpifs/phys_ec/cudlfsn.F90                      \
   arpifs/phys_ec/cududv.F90                       \
   arpifs/phys_ec/cuinin.F90                       \
   arpifs/phys_ec/cubidiag.F90                     \
   arpifs/phys_ec/cuccdia.F90                      \
   arpifs/phys_ec/cuctracer.F90                    \
   arpifs/phys_ec/cudtdqn.F90                      \
   arpifs/phys_ec/cuflxn.F90                       \
   arpifs/phys_ec/cumastrn.F90                     \
   arpifs/phys_ec/culight.F90                      \
   arpifs/phys_ec/satur.F90
do 
  echo "==> $f <=="
  dir=$(dirname $f)
  mkdir -p src/local/ifsaux/openacc/$dir
#  --only-if-newer \
  openacc.pl --interface --stack84 \
   --inlined cuadjtq.F90,cubasmcn.F90,cuentr.F90,cuadjtqs.F90 \
   --dir src/local/ifsaux/openacc/$dir \
   --nocompute ABOR1 --version \
   --jljk2jlonjlev --cycle 49 \
   $(resolve --user $f)
done

f=arpifs/adiab/smpos_parall.F90
dir=$(dirname $f)
mkdir -p src/local/ifsaux/openacc/$dir
openacc.pl --interface --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --cpg_dyn --dir src/local/ifsaux/openacc/$dir $(resolve $f)
