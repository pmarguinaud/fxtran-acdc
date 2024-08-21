#!/bin/bash


set -x
set -e

. $(dirname $0)/prolog.sh

for f in \
phyex/conv/convect_trigger_shal.F90 \
phyex/conv/convect_condens.F90 \
phyex/conv/convect_mixing_funct.F90 \
phyex/conv/convect_closure_thrvlcl.F90 \
phyex/conv/convect_closure_adjust_shal.F90 \
phyex/conv/convect_updraft_shal.F90 \
phyex/conv/convect_closure_shal.F90 
do
  echo "==> $f <=="
  openacc.pl --interface --stack84 --version --inline-contained --cycle 49 --mesonh --dir src/local/mpa/openacc $(resolve --user $f)
done

for f in \
mpa/conv/externals/shallow_convection_part1.F90 \
mpa/conv/externals/shallow_convection_part2.F90
do
  echo "==> $f <=="
  openacc.pl --interface --stack84 --version --cycle 49 --mesonh --dir src/local/mpa/openacc --modi $(resolve --user $f)
done


