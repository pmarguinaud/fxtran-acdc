#!/bin/bash

set -x
set -e

export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/cy49t1:$PATH
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

for f in \
  arpifs/adiab/cpg.F90 \
  arpifs/adiab/cpg_dia_flu.F90 \
  arpifs/adiab/cpg_end_map.F90 \
  arpifs/adiab/cpg_end_sfc.F90
do

dir=$(perl -e ' 
use File::Basename; 
my $f = shift; 
$f =~ s,^\.fypp/,,o; 
print &dirname ($f) 
' $f)

pointerParallel.pl \
  --nproma YDCPG_OPTS%KLON --cycle 49 --arpege --use-acpy \
  --types-fieldapi-dir types-fieldapi --post-parallel synchost,nullify --version --dir \
  src/local/$dir $(resolve $f)

done

for f in arpifs/dia/cpxfu.F90 arpifs/dia/cpcfu.F90
do
  dir=$(dirname $f)
  pointerParallel.pl  --inline-contains --nproma YDCPG_OPTS%KLON --cycle 49 --use-acpy  \
    --types-fieldapi-dir types-fieldapi --post-parallel synchost,nullify --version --dir src/local/$dir $(resolve $f)
done
