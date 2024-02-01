#!/bin/bash

set -x
set -e

. $(dirname $0)/prolog.sh

generateStructureMethods-fieldapi.sh $*
generateStructureMethods.sh $*

cpg.sh $*
cpg_dyn_slg.sh $*
mesonh.sh $*
shallow_mf.sh $*
openacc.sh $*
parallel.sh $*
