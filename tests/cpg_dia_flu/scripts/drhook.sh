#!/bin/bash

DR_HOOK=1 DR_HOOK_NOT_MPI=1 DR_HOOK_OPT=prof OMP_NUM_THREADS=4 ./compile.cpu_intel/wrap_cpg_dia_flux.x --case t0031 --diff --single-column

cat drhook.prof.0


