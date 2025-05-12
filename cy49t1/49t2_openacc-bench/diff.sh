#!/bin/bash

conf=typebound

diff -B -w -x '*.F90.xml' -r ref/$conf/src/local src/local
diff -B -w -x '*.F90.xml' -r ref/$conf/hub/local hub/local
