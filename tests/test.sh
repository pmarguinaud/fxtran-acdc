#/bin/bash

ARCH=NVIDIA_OPENACC

export ARCH

for ARCH in NVIDIA_OPENACC NVIDIA_OPENMPTARGET
do

  for dir in gemm gemv
  do
    cd $dir
    echo "==> $ARCH/$dir <=="
    ./$ARCH/main.x T107L070.DAT | grep -v ' OK '  | grep -v '^[ ]*$' 
    cd ..
  done
  
done
