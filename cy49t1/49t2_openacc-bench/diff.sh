#!/bin/bash

diff -B -w -x '*.F90.xml' -r ref/local src/local
