#/bin/bash

source ./env.sh

export ARCH

for ARCH in $ARCHLIST
do

  for dir in gemm gemv
  do
    cd $dir
    echo "==> $ARCH/$dir <=="
    ./$ARCH/main.x T107L070.DAT | grep -v ' OK '  | grep -v '^[ ]*$' 
    cd ..
  done
  
done
