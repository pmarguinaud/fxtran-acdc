#!/bin/bash


set -x
set -e

. $(dirname $0)/prolog.sh

for f in \
   phyex/turb/mode_thl_rt_from_th_r_mf.F90               \
   phyex/turb/mode_compute_updraft.F90                   \
   phyex/turb/mode_compute_bl89_ml.F90                   \
   phyex/turb/shuman_mf.F90                              \
   phyex/turb/mode_compute_updraft_rhcj10.F90            \
   phyex/turb/mode_compute_updraft_raha.F90              \
   phyex/turb/mode_compute_mf_cloud.F90                  \
   phyex/turb/mode_compute_mf_cloud_direct.F90           \
   phyex/turb/mode_compute_mf_cloud_stat.F90             \
   phyex/turb/mode_compute_function_thermo_mf.F90        \
   phyex/turb/mode_compute_mf_cloud_bigaus.F90           \
   phyex/turb/mode_mf_turb.F90                           \
   phyex/turb/mode_tridiag_massflux.F90                  \
   phyex/turb/mode_mf_turb_expl.F90
do
  echo "==> $f <=="
  openacc.pl --interface --stack84  --nocompute PRINT_MSG  --interfaces --version --inline-contained \
     --cycle 49 --mesonh --dir src/local/mpa/openacc $(resolve --user $f)
done

for f in \
   phyex/turb/shallow_mf.F90                             
do
  echo "==> $f <=="
  openacc.pl --interface --stack84  --nocompute PRINT_MSG  --interfaces --version --inline-contained \
     --cycle 49 --mesonh --dir src/local/mpa/openacc --modi $(resolve --user $f)
done
