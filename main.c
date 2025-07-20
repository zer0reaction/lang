#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define ABSOLUTE(a) ((a) >= 0 ? (a) : -(a))

static char *scratch_registers[] = {
  "eax", "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"
};
#define REGISTERS_COUNT (sizeof(scratch_registers) / sizeof(*scratch_registers))

static char *argument_registers[] = {
  "edi", "esi", "edx", "ecx", "r8d", "r9d"
};
#define FUNCTION_ARGS_MAX (sizeof(argument_registers) / sizeof(*argument_registers))

typedef enum{
  TT_EMPTY = 0,
  TT_IDENTIFIER,
  TT_INTEGER,
  TT_EQUAL,
  TT_PLUS,
  TT_MINUS,
  TT_ROUND_OPEN,
  TT_ROUND_CLOSE,
  TT_COUNT
}tt_t;

typedef struct{
  tt_t type;
  int id;
}token_t;

typedef struct{
  char identifier_name;
  bool visibility;
  int stack_offset;

  int integer_value;
}symbol_t;

typedef enum{
  NT_EMPTY = 0,
  NT_ASSIGNMENT,
  NT_VARIABLE,
  NT_FUNCTION_CALL,
  NT_INTEGER,
  NT_SUM,
  NT_SUBTRACTION,
  NT_COUNT
}nt_t;

typedef struct node_t node_t;
struct node_t{
  int id;
  nt_t type;
  node_t *next;

  union {
    struct {
      node_t *lval;
      node_t *rval;
    };
    struct {
      node_t *args[FUNCTION_ARGS_MAX];
      int args_count;
    };
  };
};

typedef enum{
  ST_NONE,
  ST_REGISTER,
  ST_STACK,
  ST_IMMEDIATE
}st_t;

typedef struct{
  int id;
  st_t type;

  int register_id;
}storage_t;

static symbol_t *table = NULL;

token_t *tokenize(char *s, int len)
{
  int i = 0;
  token_t *ts = NULL;

  assert(TT_COUNT == 8);

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

      case '-': {
        token_t t = {0};
        t.type = TT_MINUS;
        arrput(ts, t);
        i += 1;
      } break;

      case '(': {
        token_t t = {0};
        t.type = TT_ROUND_OPEN;
        arrput(ts, t);
        i += 1;
      } break;

      case ')': {
        token_t t = {0};
        t.type = TT_ROUND_CLOSE;
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

        // TODO: rewrite this hack
        bool found = false;
        for (int j = 0; j < arrlen(table); j++) {
          if (table[j].identifier_name == s[i]) {
            found = true;
            t.id = j;
            break;
          }
        }

        if (!found) {
          symbol_t sym = {0};
          sym.identifier_name = s[i];
          arrput(table, sym);
          t.id = arrlen(table) - 1;
        }

        arrput(ts, t);
        i += 1;
      } break;
    }
  }

  return ts;
}

void print_tokens(token_t *ts)
{
  assert(TT_COUNT == 7);

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

      case TT_ROUND_OPEN: {
        printf("( ");
      } break;

      case TT_ROUND_CLOSE: {
        printf(") ");
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
  assert(TT_COUNT == 8);
  assert(NT_COUNT == 7);

  assert(*eaten < arrlen(ts));

  node_t *n = arena_alloc(a, sizeof(*n));
  memset(n, 0, sizeof(*n));

  switch (ts[*eaten].type) {
    case TT_INTEGER: {
      n->type = NT_INTEGER;
      n->id = ts[*eaten].id;
      *eaten += 1;
    } break;

    case TT_IDENTIFIER: {
      if (arrlen(ts) - *eaten >= 3 && ts[*eaten + 1].type == TT_ROUND_OPEN) {
        n->type = NT_FUNCTION_CALL;
        n->id = ts[*eaten].id;

        *eaten += 2;

        while (ts[*eaten].type != TT_ROUND_CLOSE) {
          assert(arrlen(ts) - *eaten > 1);

          assert(n->args_count < (int)FUNCTION_ARGS_MAX);
          node_t *arg = parse(ts, eaten, a);
          n->args[n->args_count++] = arg;
        }

        *eaten += 1;
      } else {
        n->type = NT_VARIABLE;
        n->id = ts[*eaten].id;
        *eaten += 1;
      }
    } break;

    case TT_EQUAL: {
      n->type = NT_ASSIGNMENT;
      *eaten += 1;
      n->lval = parse(ts, eaten, a);
      n->rval = parse(ts, eaten, a);
    } break;

    case TT_PLUS: {
      n->type = NT_SUM;
      *eaten += 1;
      n->lval = parse(ts, eaten, a);
      n->rval = parse(ts, eaten, a);
    } break;

    case TT_MINUS: {
      n->type = NT_SUBTRACTION;
      *eaten += 1;
      n->lval = parse(ts, eaten, a);
      n->rval = parse(ts, eaten, a);
    } break;

    default: {
      assert(0 && "unknown token");
    } break;
  }

  return n;
}

void print_ast(node_t *n)
{
  assert(NT_COUNT == 7);

  if (n == NULL) return;

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

    case NT_SUBTRACTION: {
      printf("(- ");
      print_ast(n->lval);
      printf(" ");
      print_ast(n->rval);
      printf(")");
    } break;

    case NT_FUNCTION_CALL: {
      printf("%c(", table[n->id].identifier_name);
      for (int i = 0; i < n->args_count; i++) {
        print_ast(n->args[i]);
      }
      printf(") ");
    } break;

    default: {
      assert(0 && "unexpected node");
    } break;
  }

  print_ast(n->next);
}

void unwrap_storage(storage_t st)
{
  switch (st.type) {
    case ST_STACK: {
      printf("%d(%%rbp)", table[st.id].stack_offset);
    } break;

    case ST_REGISTER: {
      printf("%%%s", scratch_registers[st.register_id]);
    } break;

    case ST_IMMEDIATE: {
      printf("$%d", table[st.id].integer_value);
    } break;

    default: {
      assert(0 && "unexpected storage type");
    } break;
  }
}

storage_t codegen(node_t *n, int *registers_used, int *stack_offset)
{
  assert(NT_COUNT == 7);

  switch (n->type) {
    case NT_VARIABLE: {
      if (!table[n->id].visibility) {
        table[n->id].stack_offset = (*stack_offset -= 4);
        table[n->id].visibility = true;
      }
      return (storage_t){ .id = n->id, .type = ST_STACK };
    } break;

    case NT_INTEGER: {
      return (storage_t){ .id = n->id, .type = ST_IMMEDIATE };
    } break;

    case NT_ASSIGNMENT: {
      storage_t lval = codegen(n->lval, registers_used, stack_offset);
      storage_t rval = codegen(n->rval, registers_used, stack_offset);
      assert(lval.type != ST_NONE && rval.type != ST_NONE);

      printf("\tmovl\t");

      if (rval.type == ST_REGISTER) {
        // movl %reg, lval
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval);
        printf("\n");
        return (storage_t){ .type = ST_REGISTER,
                            .register_id = rval.register_id };
      } else {
        // movl rval, %reg
        // movl %reg, lval
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;

        unwrap_storage(rval);
        printf(", %%%s\n", scratch_registers[register_id]);
        printf("\tmovl\t%%%s, ", scratch_registers[register_id]);
        unwrap_storage(lval);
        printf("\n");

        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
     }

    } break;

    case NT_SUM: {
      storage_t lval = codegen(n->lval, registers_used, stack_offset);
      storage_t rval = codegen(n->rval, registers_used, stack_offset);
      assert(lval.type != ST_NONE && rval.type != ST_NONE);

      // addl rval, %reg
      if (lval.type == ST_REGISTER) {
        printf("\taddl\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval);
        printf("\n");
        return (storage_t){ .type = ST_REGISTER,
                            .register_id = lval.register_id };
      } else {
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;

        // movl lval, %reg
        // addl rval, %reg
        printf("\tmovl\t");
        unwrap_storage(lval);
        printf(", %%%s\n", scratch_registers[register_id]);
        printf("\taddl\t");
        unwrap_storage(rval);
        printf(", %%%s\n", scratch_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
      }
    } break;

    case NT_SUBTRACTION: {
      storage_t lval = codegen(n->lval, registers_used, stack_offset);
      storage_t rval = codegen(n->rval, registers_used, stack_offset);
      assert(lval.type != ST_NONE && rval.type != ST_NONE);

      // subl rval, %reg
      if (lval.type == ST_REGISTER) {
        printf("\tsubl\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval);
        printf("\n");
        return (storage_t){ .type = ST_REGISTER,
                            .register_id = lval.register_id };
      } else {
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;

        // movl lval, %reg
        // subl rval, %reg
        printf("\tmovl\t");
        unwrap_storage(lval);
        printf(", %%%s\n", scratch_registers[register_id]);
        printf("\tsubl\t");
        unwrap_storage(rval);
        printf(", %%%s\n", scratch_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
      }
    } break;

    case NT_FUNCTION_CALL: {
      int storage_count = n->args_count;

      for (int i = 0; i < storage_count; i++) {
        storage_t st = codegen(n->args[i], registers_used, stack_offset);
        assert(st.type != ST_NONE);

        // movl arg, reg
        printf("\tmovl\t");
        unwrap_storage(st);
        printf(", %%%s\n", argument_registers[i]);
      }

      int allign_offset = (16 - ABSOLUTE(*stack_offset) % 16) % 16;
      if (allign_offset != 0) {
        printf("\tsubq\t$%d, %%rsp\n", allign_offset);
      }
      printf("\tcall\t%c\n", table[n->id].identifier_name);
      if (allign_offset != 0) {
        printf("\taddq\t$%d, %%rsp\n", allign_offset);
      }

      return (storage_t){ .type = ST_NONE };
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

  Arena a = {0};

  FILE *fp = fopen(argv[1], "r");
  if (!fp) {
    printf("can't open file %s\n", argv[1]);
    return 1;
  }

  fseek(fp, 0, SEEK_END);
  size_t file_size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  char *s = arena_alloc(&a, sizeof(*s) * (file_size + 1));
  fread(s, 1, file_size, fp);
  s[file_size] = '\0';

  token_t *ts = tokenize(s, strlen(s));

  int eaten = 0;
  node_t *root = NULL;
  node_t *current = NULL;

  while (eaten < arrlen(ts)) {
    if (!root) {
      root = parse(ts, &eaten, &a);
      current = root;
    } else {
      current->next = parse(ts, &eaten, &a);
      current = current->next;
    }
  }

  /*
  print_ast(root);
  printf("\n");
  */

  printf(".section .text\n");
  printf(".globl _start\n");
  printf("\n");

  printf("p:\n");
  printf("\tpushq\t%%rbp\n");
  printf("\tmovq\t%%rsp, %%rbp\n");

  printf("\tmovb\t%%dil, -1(%%rbp)\n");
  printf("\tmovq\t$1, %%rax\n");
  printf("\tmovq\t$1, %%rdi\n");
  printf("\tleaq\t-1(%%rbp), %%rsi\n");
  printf("\tmovq\t$1, %%rdx\n");
  printf("\tsyscall\n");
  printf("\tleave\n");
  printf("\tret\n");
  printf("\n");

  printf("_start:\n");
  printf("\tmovq\t%%rsp, %%rbp\n");
  printf("\n");


  int registers_used = 0;
  int stack_offset = 0;
  current = root;

  while (current) {
    registers_used = 0;
    codegen(current, &registers_used, &stack_offset);
    current = current->next;
  }

  printf("\n");
  printf("\tmovq\t$60, %%rax\n");
  printf("\tmovq\t$0, %%rdi\n");
  printf("\tsyscall\n");

  arena_free(&a);
  arrfree(ts);
  arrfree(table);
  return 0;
}
