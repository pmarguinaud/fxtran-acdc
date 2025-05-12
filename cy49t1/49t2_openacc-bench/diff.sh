#!/bin/bash

conf=typebound

diff -B -w -x '*.F90.xml' -r ref/$conf/src/local run/$conf/src/local
diff -B -w -x '*.F90.xml' -r ref/$conf/hub/local run/$conf/hub/local
