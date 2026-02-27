#!/bin/bash

set -x
set -e

export ARCH

host=$(hostname)

if [ $host = "mi300x" ]
then
  ARCHLIST=AMDROCM_OPENMPTARGET
fi

if [[ $host =~ (taranis|belenos)ndl ]] 
then 
  ARCHLIST="NVIDIA_OPENACC NVIDIA_OPENMPTARGET"
fi

for ARCH in $ARCHLIST
do

  for dir in gemm gemv
  do
    cd $dir
    \rm -rf $ARCH/fxtran-acdc
    make clean 
    make boot 
    make
    cd ..
  done

done
