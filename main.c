#include <stdio.h>
#include <string.h>
#include <assert.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

/* = a 5 */

typedef enum{
  TT_EMPTY = 0,
  TT_IDENTIFIER,
  TT_INTEGER,
  TT_EQUAL,
  TT_PLUS,
  TT_COUNT // TODO: no checks
}tt_t;

typedef struct{
  tt_t type;
  int id;
}token_t;

typedef union{
  char identifier_name;
  int integer_value;
}symbol_t;

typedef enum{
  NT_EMPTY = 0,
  NT_ASSIGNMENT,
  NT_VARIABLE,
  NT_INTEGER,
  NT_SUM,
  NT_COUNT // TODO: no checks
}nt_t;

typedef struct node_t node_t;
struct node_t{
  nt_t type;
  int id;
  node_t *arg1;
  node_t *arg2;
};

static symbol_t *table = NULL;

token_t *tokenize(char *s, int len)
{
  int i = 0;
  token_t *ts = NULL;

  assert(TT_COUNT == 5);

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

      case '+': {
        token_t t = {0};
        t.type = TT_PLUS;
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
  assert(TT_COUNT == 5);

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

      case TT_PLUS: {
        printf("+ ");
      } break;

      default: {
        assert(0 && "unknown token");
      } break;
    }
  }
  printf("\n");
}

node_t *parse(token_t *ts, int *eaten, Arena *a)
{
  assert(TT_COUNT == 5);
  assert(NT_COUNT == 5);

  assert(*eaten < arrlen(ts));

  switch (ts[0 + *eaten].type) {
    case TT_INTEGER: {
      node_t *n = arena_alloc(a, sizeof(*n));
      n->type = NT_INTEGER;
      n->id = ts[0 + *eaten].id;
      *eaten += 1;
      return n;
    } break;

    case TT_IDENTIFIER: {
      node_t *n = arena_alloc(a, sizeof(*n));
      n->type = NT_VARIABLE;
      n->id = ts[0 + *eaten].id;
      *eaten += 1;
      return n;
    } break;

    case TT_EQUAL: {
      node_t *n = arena_alloc(a, sizeof(*n));
      n->type = NT_ASSIGNMENT;
      *eaten += 1;
      n->arg1 = parse(ts, eaten, a);
      n->arg2 = parse(ts, eaten, a);
      return n;
    } break;

    case TT_PLUS: {
      node_t *n = arena_alloc(a, sizeof(*n));
      n->type = NT_SUM;
      *eaten += 1;
      n->arg1 = parse(ts, eaten, a);
      n->arg2 = parse(ts, eaten, a);
      return n;
    } break;

    default: {
      assert(0 && "unknown token");
    } break;
  }
}

void print_ast(node_t *n)
{
  assert(NT_COUNT == 5);

  switch (n->type) {
    case NT_INTEGER: {
      printf("<%d>", table[n->id].integer_value);
    } break;

    case NT_VARIABLE: {
      printf("`%c`", table[n->id].identifier_name);
    } break;

    case NT_ASSIGNMENT: {
      printf("(= ");
      print_ast(n->arg1);
      printf(" ");
      print_ast(n->arg2);
      printf(")");
    } break;

    case NT_SUM: {
      printf("(+ ");
      print_ast(n->arg1);
      printf(" ");
      print_ast(n->arg2);
      printf(")");
    } break;

    default: {
      assert(0 && "unexpected node");
    } break;
  }
}

int main(void)
{
  char *s = "= a + = b 32 35";

  token_t *ts = tokenize(s, strlen(s));
  print_tokens(ts);

  Arena a = {0};
  int eaten = 0;
  node_t *root = parse(ts, &eaten, &a);
  print_ast(root);

  arena_free(&a);
  arrfree(ts);
  arrfree(table);
  return 0;
}
