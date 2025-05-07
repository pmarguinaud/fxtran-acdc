#!/bin/bash

set -x
set -e

export TARGET_PACK=$PWD

cd hub/main
find . -name '*.F90' > ../../list.hub.main
cd ../../

for f in $(cat list.hub.main)
do

  dir=$(dirname $f)

  mkdir -p hub/local/$dir

  /home/gmap/mrpm/marguina/gpupack-w/fxtran-acdc/lib/../bin/fxtran-f90 \
    --types-constant-dir $PWD/types-constant --types-fieldapi-dir $PWD/types-fieldapi \
    --cycle 49 --interface --dir hub/local/$dir --dryrun -- f90 -c hub/main/$f

done

cd src/main
find . -name '*.F90' > ../../list.src.main
cd ../..

for f in $(cat list.src.main)
do

  dir=$(dirname $f)

  mkdir -p src/local/$dir

  /home/gmap/mrpm/marguina/gpupack-w/fxtran-acdc/lib/../bin/fxtran-f90 \
    --types-constant-dir $PWD/types-constant --types-fieldapi-dir $PWD/types-fieldapi \
    --cycle 49 --interface --dir src/local/$dir --dryrun -- f90 -c src/main/$f

done
