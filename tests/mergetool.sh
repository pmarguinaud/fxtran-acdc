#!/bin/bash

export PATH=$PWD/../bin:$PATH

\rm -rf mergetool
tar zxvf mergetool.tgz
cd mergetool/
./merge-fxtran.sh

