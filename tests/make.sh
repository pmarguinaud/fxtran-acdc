#!/bin/bash

set -e

source ./env.sh

set -x

export ARCH

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
