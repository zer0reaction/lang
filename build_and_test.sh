#!/bin/bash

set -xe

./build.sh
mkdir -p .build

./lang test.lang > .build/out.s
as -o .build/out.o .build/out.s
ld -o out .build/out.o

./out
