#/bin/bash

export ARCH

if [ $(hostname) = "mi300x" ]
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
    echo "==> $ARCH/$dir <=="
    ./$ARCH/main.x T107L070.DAT | grep -v ' OK '  | grep -v '^[ ]*$' 
    cd ..
  done
  
done
