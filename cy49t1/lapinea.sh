#!/bin/bash

. /home/gmap/mrpm/marguina/fxtran-acdc/cy49t1/prolog.sh

set -x
set -e

if [ 1 -eq 1 ]
then

for f in \
  arpifs/adiab/lapinea.F90 \
  arpifs/adiab/lapineb.F90 \
  arpifs/adiab/larcinb.F90 \
  arpifs/adiab/larmes.F90
do
  dir=$(dirname $f)
  pointerParallel.pl \
    --gpumemstat --stack84 --jlon JROF --nproma YDCPG_OPTS%KLON,YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIM%NPROMNH \
    --cycle 49 --use-acpy --types-fieldapi-dir types-fieldapi --post-parallel synchost,nullify \
    --version --dir src/local/$dir $(resolve $f)
done

fi

if [ 1 -eq 1 ]
then

for f in \
  arpifs/adiab/cart2local.F90                    \
  arpifs/adiab/gnhgw2svd.F90                     \
  arpifs/adiab/gnhpre.F90                        \
  arpifs/adiab/gphpre_expl.F90                   \
  arpifs/adiab/gphpre_expl_vertfe0.F90           \
  arpifs/adiab/gphpre_expl_vertfe1.F90           \
  arpifs/adiab/gpuvs.F90                         \
  arpifs/adiab/lacone.F90                        \
  arpifs/adiab/laitre_gfl.F90                    \
  arpifs/adiab/laitre_gmv.F90                    \
  arpifs/adiab/lamaxwind.F90                     \
  arpifs/adiab/larche.F90                        \
  arpifs/adiab/larcina.F90                       \
  arpifs/adiab/latrajout.F90                     \
  arpifs/adiab/local2cart.F90                    \
  arpifs/adiab/local2cartgeov.F90                \
  arpifs/adiab/mcoords.F90                       \
  arpifs/adiab/rotuv.F90                         \
  arpifs/adiab/rotuvgeo.F90                      \
  arpifs/interpol/laiddi.F90                     \
  arpifs/interpol/laidli.F90                     \
  arpifs/interpol/laihvt.F90                     \
  arpifs/interpol/laismoo.F90                    \
  arpifs/interpol/laitli.F90                     \
  arpifs/interpol/laitlif.F90                    \
  arpifs/interpol/laitri.F90                     \
  arpifs/interpol/laitriqm3d.F90                 \
  arpifs/interpol/laitri_vector.F90              \
  arpifs/interpol/laitri_weno.F90                \
  arpifs/interpol/laitvspcqm.F90                 \
  arpifs/interpol/laqmlimiter.F90                \
  arpifs/interpol/lascaw_cla.F90                 \
  arpifs/interpol/lascaw_claturb.F90             \
  arpifs/interpol/lascaw_clo.F90                 \
  arpifs/interpol/lascaw_cloturb.F90             \
  arpifs/interpol/lascaw.F90                     \
  arpifs/interpol/lascaw_vintw.F90               \
  arpifs/interpol/laismoa.F90                    \
  arpifs/utility/verdisint.F90         
do
  dir=$(dirname $f)
  openacc.pl --interface --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --cpg_dyn --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)
done

fi

if [ 1 -eq 1 ]
then

for f in \
  arpifs/utility/verint.F90                      \
  arpifs/utility/verder.F90                      \
  arpifs/utility/verints.F90
do
  dir=$(dirname $f)
  openacc.pl \
    --interface --set-variables 'LLVERINT_ON_CPU=.FALSE.,LLSIMPLE_DGEMM=.TRUE.' \
    --no-check-pointers-dims ZIN,ZOUT \
    --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --cpg_dyn --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)
done

fi

if [ 1 -eq 1 ]
then

for f in \
  arpifs/interpol/laitri_scalar.F90              \
  aladin/adiab/elarche.F90                       \
  aladin/interpol/elascaw.F90
do
  dir=$(dirname $f)
  openacc.pl --interface --dummy --stack84 --cycle 49 --pointers --nocompute ABOR1 --version --cpg_dyn --dir src/local/ifsaux/openacc/$dir $(resolve --user $f)
done

fi
