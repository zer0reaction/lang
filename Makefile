CC=gcc
CFLAGS=-Wall -Wextra -std=c99 -fsanitize=address,undefined

all: lang

lang: lang.c
	${CC} ${CFLAGS} -o lang lang.c

test: lang test.lang
	./lang test.lang > .build/test.s
	as -o .build/test.o .build/test.s
	ld -o test .build/test.o
