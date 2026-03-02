#/bin/bash

source ./env.sh

export ARCH

for ARCH in $ARCHLIST
do
  echo "==> $ARCH <=="
  ./$ARCH/main.x T107L070 | grep -v ' OK '  | grep -v '^[ ]*$' 
done
