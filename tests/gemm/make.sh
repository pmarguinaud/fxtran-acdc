#!/bin/bash

set -e

source ./env.sh

set -x

export ARCH

for ARCH in $ARCHLIST
do
 \rm -rf $ARCH/fxtran-acdc
 make clean 
 make boot 
 make
done
