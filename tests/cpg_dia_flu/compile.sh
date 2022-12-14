#!/bin/bash

set -x

./scripts/compile.pl \
  --arch cpu_intel --update --compile --external-drhook
