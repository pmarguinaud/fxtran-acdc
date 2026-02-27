#!/bin/bash

set -x
set -e

export ARCH

for ARCH in NVIDIA_OPENACC NVIDIA_OPENMPTARGET
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
