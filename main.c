#include <stdio.h>
#include <string.h>
#include <assert.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

typedef enum{
  TT_EMPTY = 0,
  TT_IDENTIFIER,
  TT_INTEGER,
  TT_EQUAL,
  TT_PLUS,
  TT_COUNT
}tt_t;

typedef struct{
  tt_t type;
  int id;
}token_t;

typedef union{
  char identifier_name;
  int integer_value;
  int stack_offset;
}symbol_t;

typedef enum{
  NT_EMPTY = 0,
  NT_ASSIGNMENT,
  NT_VARIABLE,
  NT_INTEGER,
  NT_SUM,
  NT_COUNT
}nt_t;

typedef struct node_t node_t;
struct node_t{
  nt_t type;
  int id;
  node_t *lval;
  node_t *rval;
};

typedef enum{
  ST_NONE,
  ST_REGISTER,
  ST_STACK
}st_t;

typedef struct{
  st_t type;
  union {
    int register_id;
    int stack_offset;
  };
}storage_t;

static symbol_t *table = NULL;

static char *registers[] = {
  "eax", "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"
};
#define REGISTERS_COUNT (sizeof(registers) / sizeof(*registers))

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
      n->lval = parse(ts, eaten, a);
      n->rval = parse(ts, eaten, a);
      return n;
    } break;

    case TT_PLUS: {
      node_t *n = arena_alloc(a, sizeof(*n));
      n->type = NT_SUM;
      *eaten += 1;
      n->lval = parse(ts, eaten, a);
      n->rval = parse(ts, eaten, a);
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
      print_ast(n->lval);
      printf(" ");
      print_ast(n->rval);
      printf(")");
    } break;

    case NT_SUM: {
      printf("(+ ");
      print_ast(n->lval);
      printf(" ");
      print_ast(n->rval);
      printf(")");
    } break;

    default: {
      assert(0 && "unexpected node");
    } break;
  }
}

/*

- check if the variable is defined (context_variable_ids array)

- create IR (array of structs with register names, variable names, literals)
  left side (arg1) first (because of allocation)
  add variable's offset to symbol table

- semantic analysis

*/

storage_t codegen(node_t *n, int *registers_used, int *stack_offset) {
  switch (n->type) {
    // stack
    case NT_VARIABLE: {
      // TODO: check if variable exists, currently only creates new variable
      int variable_offset = (*stack_offset -= 4);
      table[n->id].stack_offset = variable_offset;
      return (storage_t){ .type = ST_STACK, .stack_offset = variable_offset };
    } break;

    // register
    case NT_INTEGER: {
      assert(*registers_used < (int)REGISTERS_COUNT);

      int register_id = (*registers_used)++;
      int value = table[n->id].integer_value;

      printf("\tmovl\t$%d, %%%s\n", value, registers[register_id]);
      return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
    } break;

    // none
    case NT_ASSIGNMENT: {
      storage_t lval = codegen(n->lval, registers_used, stack_offset);
      storage_t rval = codegen(n->rval, registers_used, stack_offset);

      assert(lval.type != ST_NONE && rval.type != ST_NONE);

      // movl rval, lval

      printf("\tmovl\t");

      switch (rval.type) {
        case ST_STACK: {
          printf("%d(%%rbp)", rval.stack_offset);
        } break;

        case ST_REGISTER: {
          printf("%%%s", registers[rval.register_id]);
        } break;

        default: {
          assert(0 && "unexpected storage type");
        } break;
      }

      printf(", ");

      switch (lval.type) {
        case ST_STACK: {
          printf("%d(%%rbp)", lval.stack_offset);
        } break;

        case ST_REGISTER: {
          printf("%%%s", registers[lval.register_id]);
        } break;

        default: {
          assert(0 && "unexpected storage type");
        } break;
      }

      printf("\n");

      return (storage_t){ .type = ST_NONE };
    } break;

    // register
    case NT_SUM: {
      storage_t lval = codegen(n->lval, registers_used, stack_offset);
      storage_t rval = codegen(n->rval, registers_used, stack_offset);
      assert(lval.type != ST_NONE && rval.type != ST_NONE);

      assert(*registers_used < (int)REGISTERS_COUNT);
      int register_id = (*registers_used)++;

      // movl lval, %reg
      // addl rval, %reg

      printf("\tmovl\t");

      switch (lval.type) {
        case ST_STACK: {
          printf("%d(%%rbp)", lval.stack_offset);
        } break;

        case ST_REGISTER: {
          printf("%%%s", registers[lval.register_id]);
        } break;

        default: {
          assert(0 && "unexpected storage type");
        } break;
      }

      printf(", %%%s\n", registers[register_id]);
      printf("\taddl\t");

      switch (rval.type) {
        case ST_STACK: {
          printf("%d(%%rbp)", rval.stack_offset);
        } break;

        case ST_REGISTER: {
          printf("%%%s", registers[rval.register_id]);
        } break;

        default: {
          assert(0 && "unexpected storage type");
        } break;
      }

      printf(", %%%s\n", registers[register_id]);

      return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
    } break;

    default: {
      assert(0 && "unexpected node");
    } break;
  }
}

int main(int argc, char *argv[])
{
  if (argc != 2) {
    printf("incorrect number of arguments\n");
    return 1;
  }

  char *s = argv[1];

  token_t *ts = tokenize(s, strlen(s));

  Arena a = {0};
  int eaten = 0;
  node_t **ns = NULL;

  while (eaten < arrlen(ts)) {
    node_t *n = parse(ts, &eaten, &a);
    arrput(ns, n);
  }

  /*
  for (int i = 0; i < arrlen(ns); i++) {
    print_ast(ns[i]);
    printf("\n");
  }
  */

  int registers_used = 0;
  int stack_offset = 0;
  for (int i = 0; i < arrlen(ns); i++) {
    registers_used = 0;
    codegen(ns[i], &registers_used, &stack_offset);
  }

  arena_free(&a);
  arrfree(ts);
  arrfree(ns);
  arrfree(table);
  return 0;
}
