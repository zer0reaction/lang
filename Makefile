CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99

all: main

main: main.c
	${CC} ${CFLAGS} -o main main.c
