#!/bin/bash


export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/bin:$PATH

function resolve ()
{
  f=$1
  for view in $(cat .gmkview)
  do
    g="src/$view/$f"
    if [ -f $g ]
    then
      echo $g
      break
    fi
  done
}

set -x
set -e

for f in \
mpa/conv/internals/convect_satmixratio.F90 \
mpa/conv/internals/convect_trigger_shal.F90 \
mpa/conv/internals/convect_condens.F90 \
mpa/conv/internals/convect_mixing_funct.F90 \
mpa/conv/internals/convect_closure_thrvlcl.F90 \
mpa/conv/internals/convect_closure_adjust_shal.F90 \
mpa/conv/internals/convect_updraft_shal.F90 \
mpa/conv/internals/convect_closure_shal.F90 
do
  echo "==> $f <=="
  openacc.pl --version --inlined convect_satmixratio.F90 --cycle 49 --mesonh --dir src/local/mpa/openacc $(resolve $f)
done

for f in \
mpa/conv/externals/shallow_convection_part1.F90 \
mpa/conv/externals/shallow_convection_part2.F90
do
  echo "==> $f <=="
  openacc.pl --version --cycle 49 --mesonh --dir src/local/mpa/openacc --modi $(resolve $f)
done


