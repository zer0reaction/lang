CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -fsanitize=address,undefined

all: main

main: main.c
	${CC} ${CFLAGS} -o main main.c
