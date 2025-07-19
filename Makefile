CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -fsanitize=address,undefined

all: main

main: main.c
	${CC} ${CFLAGS} -o main main.c

test: main test.lang
	./main test.lang > .build/test.s
	as -o .build/test.o .build/test.s
	ld -o test .build/test.o
