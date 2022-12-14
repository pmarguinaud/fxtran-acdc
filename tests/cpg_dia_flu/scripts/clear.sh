#!/bin/bash

set -x

for f in compute/*.F90
do 

  b=$(basename $f .F90)
  \rm -f compile.*/$b*

done
