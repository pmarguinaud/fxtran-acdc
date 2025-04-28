#!/bin/bash

find src/main/ -name '*.F90.xml' | xargs rm -f
find src/local/ -type f | xargs rm -f

\rm -rf types-constant types-fieldapi *.xml
