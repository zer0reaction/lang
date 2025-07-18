#include <stdio.h>
#include <string.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

/* = a 5 */

typedef enum{
  TT_EMPTY = 0,
  TT_IDENTIFIER,
  TT_INTEGER,
  TT_EQUAL,
  TT_COUNT
}tt_t;

typedef struct{
  tt_t type;
  int id;
}token_t;

typedef struct{
  char identifier_name;
  int integer_value;
}symbol_t;

static symbol_t *table = NULL;

token_t *tokenize(char *s, int len)
{
  int i = 0;
  token_t *ts = NULL;

  while (i < len) {
    switch (s[i]) {
    case ' ':
    case '\t':
    case '\n': {
      i += 1;
    } break;

    case '=': {
      token_t t = {0};
      t.type = TT_EQUAL;
      arrput(ts, t);
      i += 1;
    } break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
      int value = 0;
      while (i < len && s[i] >= '0' && s[i] <= '9') {
        value *= 10;
        value += s[i] - '0';
        i += 1;
      }

      token_t t = {0};
      t.type = TT_INTEGER;

      symbol_t sym = {0};
      sym.integer_value = value;
      arrput(table, sym);

      t.id = arrlen(table) - 1;
      arrput(ts, t);
    } break;

    default: {
      token_t t = {0};
      t.type = TT_IDENTIFIER;

      symbol_t sym = {0};
      sym.identifier_name = s[i];
      arrput(table, sym);

      t.id = arrlen(table) - 1;

      arrput(ts, t);
      i += 1;
    } break;
    }
  }

  return ts;
}

void print_tokens(token_t *ts)
{
  for (int i = 0; i < arrlen(ts); i++) {
    switch (ts[i].type) {
    case TT_IDENTIFIER: {
      printf("`%c` ", table[ts[i].id].identifier_name);
    } break;

    case TT_INTEGER: {
      printf("<%d> ", table[ts[i].id].integer_value);
    } break;

    case TT_EQUAL: {
      printf("= ");
    } break;

    default: {
      printf("UNKNOWN TOKEN ");
    } break;
    }
  }
  printf("\n");
}

int main(void)
{
  char *s = "= a 5\n= b 69\n";

  token_t *ts = tokenize(s, strlen(s));
  print_tokens(ts);

  arrfree(ts);
  arrfree(table);
  return 0;
}
