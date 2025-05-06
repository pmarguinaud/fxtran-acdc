#!/bin/bash

diff -B -w -x '*.F90.xml' -r ref/src/local src/local
diff -B -w -x '*.F90.xml' -r ref/hub/local hub/local
