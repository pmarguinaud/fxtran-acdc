#!/bin/bash

set -e
set -x

. $(dirname $0)/prolog.sh

for f in \
  arpifs/phys_radi/ac_cloud_model2.F90  \
  arpifs/phys_radi/acraneb_transt.F90   \
  arpifs/phys_radi/acraneb_transs.F90   \
  arpifs/phys_radi/acraneb_coefs.F90    \
  arpifs/phys_radi/acraneb_coeft.F90    \
  arpifs/phys_radi/acraneb_solvs.F90    \
  arpifs/phys_radi/acraneb_solvt.F90    \
  arpifs/phys_radi/acraneb_solvt1.F90   \
  arpifs/phys_radi/acraneb_solvt3.F90   \
  arpifs/phys_radi/acraneb2.F90
do 
  echo "==> $f <=="
  dir=$(dirname $f)
  mkdir -p src/local/ifsaux/openacc/$dir
#  --only-if-newer 
  openacc.pl \
   --interface \
   --stack84 --inline-contained --acraneb2 \
   --dir src/local/ifsaux/openacc/$dir \
   --nocompute ABOR1 --version \
   $(resolve --user $f)
done


