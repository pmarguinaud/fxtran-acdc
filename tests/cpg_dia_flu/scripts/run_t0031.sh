#!/bin/bash

set -x

\rm -f drhook.prof.0

cd /home/gmap/mrpm/marguina/cpg_dia_flu
DR_HOOK=1 DR_HOOK_NOT_MPI=1 DR_HOOK_OPT=prof \
 OMP_NUM_THREADS=4 ./compile.cpu_intel/wrap_cpg_dia_flux.x --case t0031 --parallel-field-api --diff

cat drhook.prof.0

