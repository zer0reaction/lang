#!/bin/bash

CC='gcc'
CFLAGS='-Wall -Wextra -std=c99 -fsanitize=address,undefined'

set -xe

${CC} ${CFLAGS} -o lang lang.c
