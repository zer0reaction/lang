#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define SCOPE_IDS_BUF_SIZE 32
#define IDENT_NAME_MAX_LEN 32

#define ABSOLUTE(a) ((a) >= 0 ? (a) : -(a))

static char *scratch_4b_registers[] = {
    "eax", "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"
};
static char *scratch_1b_registers[] = {
    "al", "dil", "sil", "dl", "cl", "r8b", "r9b", "r10b", "r11b"
};
#define REGISTERS_COUNT (sizeof(scratch_4b_registers) / sizeof(*scratch_4b_registers))

static char *argument_registers[] = {
    "edi", "esi", "edx", "ecx", "r8d", "r9d"
};
#define FUNCTION_ARGS_MAX (sizeof(argument_registers) / sizeof(*argument_registers))

typedef enum{
    TT_EMPTY = 0,
    TT_IDENT,
    TT_INT,
    TT_EQUAL,
    TT_CMP_EQ,
    TT_CMP_NEQ,
    TT_PLUS,
    TT_MINUS,
    TT_LET,
    TT_WHILE,
    TT_IF,
    TT_ELSE,
    TT_ROUND_OPEN,
    TT_ROUND_CLOSE,
    TT_CURLY_OPEN,
    TT_CURLY_CLOSE,
    TT_COUNT
}tt_t;

typedef struct{
    tt_t type;
    union {
        char ident_name[IDENT_NAME_MAX_LEN + 1];
        int int_value;
    };
}token_t;

typedef struct{
    char ident_name[IDENT_NAME_MAX_LEN + 1];
    int stack_offset;
    int scope_id;
}symbol_t;

typedef enum{
    NT_EMPTY = 0,
    NT_ASSIGN,
    NT_CMP_EQ,
    NT_CMP_NEQ,
    NT_VAR,
    NT_DECL,
    NT_WHILE,
    NT_IF,
    NT_FUNC_CALL,
    NT_SCOPE,
    NT_INT,
    NT_SUM,
    NT_SUB,
    NT_COUNT
}nt_t;

typedef struct node_t node_t;
struct node_t{
    nt_t type;
    int table_id;
    node_t *next;

    union {
        int int_value;
        char var_name[IDENT_NAME_MAX_LEN + 1];
        struct {
            node_t *while_cond;
            node_t *while_body;
        };
        struct {
            node_t *if_cond;
            node_t *if_body;
            node_t *else_body;
        };
        struct {
            int scope_id;
            node_t *scope_start;
        };
        struct {
            node_t *lval;
            node_t *rval;
        };
        struct {
            char func_name[IDENT_NAME_MAX_LEN + 1];
            node_t *args[FUNCTION_ARGS_MAX];
            int args_count;
            int allign_offset;
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
    st_t type;
    union {
        int table_id;
        int register_id;
        int int_value;
    };
}storage_t;

static symbol_t *table = NULL;

token_t *tokenize(char *s, int len)
{
    int i = 0;
    token_t *ts = NULL;

    assert(TT_COUNT == 16);

    while (i < len) {
        if (len - i >= (int)strlen("let") && strncmp(&s[i], "let", (int)strlen("let")) == 0) {
            token_t t = {0};
            t.type = TT_LET;
            arrput(ts, t);
            i += strlen("let");
        }
        else if (len - i >= (int)strlen("while") && strncmp(&s[i], "while", (int)strlen("while")) == 0) {
            token_t t = {0};
            t.type = TT_WHILE;
            arrput(ts, t);
            i += strlen("while");
        }
        else if (len - i >= (int)strlen("if") && strncmp(&s[i], "if", (int)strlen("if")) == 0) {
            token_t t = {0};
            t.type = TT_IF;
            arrput(ts, t);
            i += strlen("if");
        }
        else if (len - i >= (int)strlen("else") && strncmp(&s[i], "else", (int)strlen("else")) == 0) {
            token_t t = {0};
            t.type = TT_ELSE;
            arrput(ts, t);
            i += strlen("else");
        }
        else if (len - i >= (int)strlen("==") && strncmp(&s[i], "==", (int)strlen("==")) == 0) {
            token_t t = {0};
            t.type = TT_CMP_EQ;
            arrput(ts, t);
            i += strlen("==");
        }
        else if (len - i >= (int)strlen("!=") && strncmp(&s[i], "!=", (int)strlen("!=")) == 0) {
            token_t t = {0};
            t.type = TT_CMP_NEQ;
            arrput(ts, t);
            i += strlen("!=");
        }
        else if (s[i] >= 'a' && s[i] <= 'z') {
            token_t t = {0};
            t.type = TT_IDENT;

            int name_len = 0;
            while (len - i > 0 && ((s[i] >= 'a' && s[i] <= 'z') || s[i] == '_')) {
                assert(name_len < IDENT_NAME_MAX_LEN);
                t.ident_name[name_len++] = s[i++];
            }

            arrput(ts, t);
        }

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

        case '{': {
            token_t t = {0};
            t.type = TT_CURLY_OPEN;
            arrput(ts, t);
            i += 1;
        } break;

        case '}': {
            token_t t = {0};
            t.type = TT_CURLY_CLOSE;
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
            t.type = TT_INT;
            t.int_value = value;

            arrput(ts, t);
        } break;

        default:
            assert(0 && "unexpected character");
        }
    }

    return ts;
}

void print_tokens(token_t *ts)
{
    assert(TT_COUNT == 10);

    for (int i = 0; i < arrlen(ts); i++) {
        switch (ts[i].type) {
        case TT_IDENT: {
            printf("`%s` ", ts[i].ident_name);
        } break;

        case TT_INT: {
            printf("<%d> ", ts[i].int_value);
        } break;

        case TT_EQUAL: {
            printf("= ");
        } break;

        case TT_PLUS: {
            printf("+ ");
        } break;

        case TT_MINUS: {
            printf("- ");
        } break;

        case TT_ROUND_OPEN: {
            printf("( ");
        } break;

        case TT_ROUND_CLOSE: {
            printf(") ");
        } break;

        case TT_CURLY_OPEN: {
            printf("{ ");
        } break;

        case TT_CURLY_CLOSE: {
            printf("} ");
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
    assert(TT_COUNT == 16);
    assert(NT_COUNT == 13);

    assert(*eaten < arrlen(ts));

    node_t *n = arena_alloc(a, sizeof(*n));
    memset(n, 0, sizeof(*n));

    switch (ts[*eaten].type) {
    case TT_INT: {
        n->type = NT_INT;
        n->int_value = ts[*eaten].int_value;
        *eaten += 1;
    } break;

    case TT_LET: {
        assert(arrlen(ts) - *eaten >= 2);
        assert(ts[*eaten + 1].type == TT_IDENT);

        n->type = NT_DECL;
        strcpy(n->var_name, ts[*eaten + 1].ident_name);
        *eaten += 2;
    } break;

    case TT_WHILE: {
        n->type = NT_WHILE;
        *eaten += 1;
        n->while_cond = parse(ts, eaten, a);
        n->while_body = parse(ts, eaten, a);
    } break;

    case TT_IF: {
        n->type = NT_IF;
        *eaten += 1;
        n->if_cond = parse(ts, eaten, a);
        n->if_body = parse(ts, eaten, a);

        if (*eaten < arrlen(ts) && ts[*eaten].type == TT_ELSE) {
            *eaten += 1;
            n->else_body = parse(ts, eaten, a);
        }
    } break;

    case TT_IDENT: {
        if (arrlen(ts) - *eaten >= 3 && ts[*eaten + 1].type == TT_ROUND_OPEN) {
            n->type = NT_FUNC_CALL;
            strcpy(n->func_name, ts[*eaten].ident_name);
            *eaten += 2;

            while (ts[*eaten].type != TT_ROUND_CLOSE) {
                assert(arrlen(ts) - *eaten > 1);
                assert(n->args_count < (int)FUNCTION_ARGS_MAX);
                node_t *arg = parse(ts, eaten, a);
                n->args[n->args_count++] = arg;
            }

            *eaten += 1;
        } else {
            n->type = NT_VAR;
            strcpy(n->var_name, ts[*eaten].ident_name);
            *eaten += 1;
        }
    } break;

    case TT_EQUAL: {
        n->type = NT_ASSIGN;
        *eaten += 1;
        n->lval = parse(ts, eaten, a);
        n->rval = parse(ts, eaten, a);
    } break;

    case TT_CMP_EQ: {
        n->type = NT_CMP_EQ;
        *eaten += 1;
        n->lval = parse(ts, eaten, a);
        n->rval = parse(ts, eaten, a);
    } break;

    case TT_CMP_NEQ: {
        n->type = NT_CMP_NEQ;
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
        n->type = NT_SUB;
        *eaten += 1;
        n->lval = parse(ts, eaten, a);
        n->rval = parse(ts, eaten, a);
    } break;

    case TT_CURLY_OPEN: {
        n->type = NT_SCOPE;
        *eaten += 1;
        node_t *current = NULL;
        while (ts[*eaten].type != TT_CURLY_CLOSE) {
            assert(arrlen(ts) - *eaten > 1);
            node_t *node = parse(ts, eaten, a);
            if (!n->scope_start) {
                n->scope_start = node;
                current = node;
            } else {
                current->next = node;
                current = current->next;
            }
        }
        *eaten += 1;
    } break;

    default: {
        printf("%d\n", ts[*eaten].type);
        assert(0 && "unknown token");
    } break;
    }

    return n;
}

void print_ast(node_t *n)
{
    assert(NT_COUNT == 10);

    if (n == NULL) return;

    switch (n->type) {
    case NT_INT: {
        printf("<%d>", n->int_value);
    } break;

    case NT_VAR: {
        printf("[%d]`%s`@%d ", n->table_id, table[n->table_id].ident_name,
               table[n->table_id].stack_offset);
    } break;

    case NT_ASSIGN: {
        printf("(= ");
        print_ast(n->lval);
        printf(" ");
        print_ast(n->rval);
        printf(") ");
    } break;

    case NT_SUM: {
        printf("(+ ");
        print_ast(n->lval);
        printf(" ");
        print_ast(n->rval);
        printf(") ");
    } break;

    case NT_SUB: {
        printf("(- ");
        print_ast(n->lval);
        printf(" ");
        print_ast(n->rval);
        printf(") ");
    } break;

    case NT_FUNC_CALL: {
        printf("%s(%d)(", n->func_name, n->allign_offset);
        for (int i = 0; i < n->args_count; i++) {
            print_ast(n->args[i]);
        }
        printf(") ");
    } break;

    case NT_SCOPE: {
        printf("{(%d) ", n->scope_id);
        print_ast(n->scope_start);
        printf("}\n");
    } break;

    case NT_DECL: {
        printf("Let[%d]`%s`@%d ", n->table_id, table[n->table_id].ident_name,
               table[n->table_id].stack_offset);
    } break;

    case NT_WHILE: {
        printf("While");
        print_ast(n->while_cond);
        print_ast(n->while_body);
    } break;

    default: {
        assert(0 && "unexpected node");
    } break;
    }

    print_ast(n->next);
}

void scope_pass(node_t *n, int *scope_count)
{
    assert(NT_COUNT == 13);

    if (!n) return;

    switch (n->type) {
    case NT_SCOPE: {
        n->scope_id = ++(*scope_count);
        scope_pass(n->scope_start, scope_count);
    } break;

    case NT_SUM:
    case NT_SUB:
    case NT_CMP_EQ:
    case NT_CMP_NEQ:
    case NT_ASSIGN: {
        scope_pass(n->lval, scope_count);
        scope_pass(n->rval, scope_count);
    } break;

    case NT_WHILE: {
        scope_pass(n->while_cond, scope_count);
        scope_pass(n->while_body, scope_count);
    } break;

    case NT_IF: {
        scope_pass(n->if_cond, scope_count);
        scope_pass(n->if_body, scope_count);
        scope_pass(n->else_body, scope_count);
    } break;

    case NT_FUNC_CALL: {
        for (int i = 0; i < n->args_count; i++) {
            scope_pass(n->args[i], scope_count);
        }
    } break;

    default:
        break;
    }

    scope_pass(n->next, scope_count);
}

void var_pass(node_t *n, int *stack_offset, int scope_ids[], int size)
{
    assert(NT_COUNT == 13);

    if (!n) return;

    switch (n->type) {
    case NT_DECL: {
        assert(size >= 1);
        bool in_current_scope = false;
        int current_scope_id = scope_ids[size - 1];

        for (int i = 0; i < arrlen(table); i++) {
            if (table[i].scope_id == current_scope_id &&
                n->var_name == table[i].ident_name) {
                in_current_scope = true;
                break;
            }
        }

        assert(!in_current_scope);

        symbol_t sym = {0};
        strcpy(sym.ident_name, n->var_name);
        sym.stack_offset = (*stack_offset -= 4);
        sym.scope_id = current_scope_id;
        arrput(table, sym);
        n->table_id = arrlen(table) - 1;
    } break;

    case NT_VAR: {
        bool visible = false;
        int table_id = 0;
        for (int i = 0; i < arrlen(table); i++) {
            if (strcmp(n->var_name, table[i].ident_name) == 0) {
                for (int j = size - 1; j >= 0; j--) {
                    if (scope_ids[j] == table[i].scope_id) {
                        visible = true;
                        table_id = i;
                        break;
                    }
                }
            }
        }
        assert(visible);
        n->table_id = table_id;
    } break;

    case NT_SCOPE: {
        assert(size < SCOPE_IDS_BUF_SIZE);
        int new_scope_ids[SCOPE_IDS_BUF_SIZE] = {0};
        for (int i = 0; i < size; i++) {
            new_scope_ids[i] = scope_ids[i];
        }
        new_scope_ids[size] = n->scope_id;
        var_pass(n->scope_start, stack_offset, new_scope_ids, size + 1);
    } break;

    case NT_SUM:
    case NT_SUB:
    case NT_CMP_EQ:
    case NT_CMP_NEQ:
    case NT_ASSIGN: {
        var_pass(n->lval, stack_offset, scope_ids, size);
        var_pass(n->rval, stack_offset, scope_ids, size);
    } break;

    case NT_WHILE: {
        var_pass(n->while_cond, stack_offset, scope_ids, size);
        var_pass(n->while_body, stack_offset, scope_ids, size);
    } break;

    case NT_IF: {
        var_pass(n->if_cond, stack_offset, scope_ids, size);
        var_pass(n->if_body, stack_offset, scope_ids, size);
        var_pass(n->else_body, stack_offset, scope_ids, size);
    } break;

    case NT_FUNC_CALL: {
        n->allign_offset = ABSOLUTE(*stack_offset) +
                           (16 - ABSOLUTE(*stack_offset) % 16) % 16;
        for (int i = 0; i < n->args_count; i++) {
            var_pass(n->args[i], stack_offset, scope_ids, size);
        }
    } break;

    default:
        break;
    }

    var_pass(n->next, stack_offset, scope_ids, size);
}

void unwrap_storage(storage_t st)
{
    switch (st.type) {
    case ST_STACK: {
        printf("%d(%%rbp)", table[st.table_id].stack_offset);
    } break;

    case ST_REGISTER: {
        printf("%%%s", scratch_4b_registers[st.register_id]);
    } break;

    case ST_IMMEDIATE: {
        printf("$%d", st.int_value);
    } break;

    default: {
        assert(0 && "unexpected storage type");
    } break;
    }
}

/*
  immediate -> move to register
  register  -> do nothing
  stack     -> move to register
*/
storage_t move_to_register(storage_t st, int *registers_used)
{
    switch (st.type) {
    case ST_REGISTER: {
        return st;
    } break;

    case ST_IMMEDIATE: {
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;
        printf("\tmovl\t$%d, %%%s\n", st.int_value, scratch_4b_registers[register_id]);
        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
    } break;

    case ST_STACK: {
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;
        printf("\tmovl\t%d(%%rbp), %%%s\n", table[st.table_id].stack_offset, scratch_4b_registers[register_id]);
        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
    } break;

    default:
        assert(0 && "unknown storage type");
    }
}

/*
  immediate -> move to register
  register  -> do nothing
  stack     -> do nothing
*/
storage_t make_mutable(storage_t st, int *registers_used)
{
    switch (st.type) {
    case ST_IMMEDIATE: {
        assert(*registers_used < (int)REGISTERS_COUNT);
        int register_id = (*registers_used)++;
        printf("\tmovl\t$%d, %%%s\n", st.int_value, scratch_4b_registers[register_id]);
        return (storage_t){ .type = ST_REGISTER, .register_id = register_id };
    } break;

    case ST_REGISTER: {
        return st;
    } break;

    case ST_STACK: {
        return st;
    } break;

    default:
        assert(0 && "unknown storage type");
    }
}

storage_t codegen(node_t *n, int *registers_used)
{
    assert(NT_COUNT == 13);

    switch (n->type) {
    case NT_DECL:
    case NT_VAR: {
        return (storage_t){ .table_id = n->table_id, .type = ST_STACK };
    } break;

    case NT_INT: {
        return (storage_t){ .type = ST_IMMEDIATE, .int_value = n->int_value };
    } break;

    case NT_ASSIGN: {
        storage_t lval = codegen(n->lval, registers_used);
        storage_t rval_init = codegen(n->rval, registers_used);
        assert(lval.type != ST_NONE && rval_init.type != ST_NONE);

        storage_t rval_reg = move_to_register(rval_init, registers_used);

        printf("\tmovl\t");
        unwrap_storage(rval_reg);
        printf(", ");
        unwrap_storage(lval);
        printf("\n");

        return rval_reg;
    } break;

    case NT_CMP_EQ: {
        storage_t lval_init = codegen(n->lval, registers_used);
        storage_t rval = codegen(n->rval, registers_used);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = move_to_register(lval_init, registers_used);

        printf("\tcmp\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval_reg);
        printf("\n");
        printf("\tsete\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        unwrap_storage(lval_reg);
        printf("\n");

        return lval_reg;
    } break;

    case NT_CMP_NEQ: {
        storage_t lval_init = codegen(n->lval, registers_used);
        storage_t rval = codegen(n->rval, registers_used);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = move_to_register(lval_init, registers_used);

        printf("\tcmp\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval_reg);
        printf("\n");
        printf("\tsetne\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        unwrap_storage(lval_reg);
        printf("\n");

        return lval_reg;
    } break;

    case NT_SUM: {
        storage_t lval_init = codegen(n->lval, registers_used);
        storage_t rval = codegen(n->rval, registers_used);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = move_to_register(lval_init, registers_used);

        printf("\taddl\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval_reg);
        printf("\n");

        return lval_reg;
    } break;

    case NT_SUB: {
        storage_t lval_init = codegen(n->lval, registers_used);
        storage_t rval = codegen(n->rval, registers_used);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = move_to_register(lval_init, registers_used);

        printf("\tsubl\t");
        unwrap_storage(rval);
        printf(", ");
        unwrap_storage(lval_reg);
        printf("\n");

        return lval_reg;
    } break;

    case NT_FUNC_CALL: {
        int storage_count = n->args_count;

        for (int i = 0; i < storage_count; i++) {
            storage_t st = codegen(n->args[i], registers_used);
            assert(st.type != ST_NONE);

            printf("\tmovl\t");
            unwrap_storage(st);
            printf(", %%%s\n", argument_registers[i]);
        }

        if (n->allign_offset != 0) {
            printf("\tsubq\t$%d, %%rsp\n", n->allign_offset);
        }
        printf("\tcall\t%s\n", n->func_name);
        if (n->allign_offset != 0) {
            printf("\taddq\t$%d, %%rsp\n", n->allign_offset); // TODO: refactor, lame
        }

        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_SCOPE: {
        node_t *current = n->scope_start;
        while (current) {
            codegen(current, registers_used);
            current = current->next;
        }
        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_WHILE: {
        unsigned ident = rand(); // TODO: can possibly collide

        printf(".while_start_%u:\n", ident);

        storage_t cond_init = codegen(n->while_cond, registers_used);
        assert(cond_init.type != ST_NONE);
        storage_t cond_mut = make_mutable(cond_init, registers_used);

        printf("\tcmpl\t$0, ");
        unwrap_storage(cond_mut);
        printf("\n");
        printf("\tjz\t.while_end_%u\n", ident);

        codegen(n->while_body, registers_used);

        printf("\tjmp\t.while_start_%u\n", ident);
        printf(".while_end_%u:\n", ident);

        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_IF: {
        unsigned ident = rand(); // TODO: can possibly collide

        storage_t cond_init = codegen(n->if_cond, registers_used);
        assert(cond_init.type != ST_NONE);
        storage_t cond_mut = make_mutable(cond_init, registers_used);

        printf("\tcmpl\t$0, ");
        unwrap_storage(cond_mut);
        printf("\n");
        printf("\tjz\t.if_end_%d\n", ident);
        codegen(n->if_body, registers_used);
        printf(".if_end_%d:\n", ident);

        if (n->else_body) {
            printf("\tcmpl\t$0, ");
            unwrap_storage(cond_mut);
            printf("\n");
            printf("\tjnz\t.else_end_%d\n", ident);
            codegen(n->else_body, registers_used);
            printf(".else_end_%d:\n", ident);
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
    /*
    print_tokens(ts);
    */

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

    int scope_count = 0;
    scope_pass(root, &scope_count);

    int stack_offset = 0;
    int scope_ids[SCOPE_IDS_BUF_SIZE] = {0};
    var_pass(root, &stack_offset, scope_ids, 1);

/*
    print_ast(root);
    printf("\n");
*/

    printf(".section .text\n");
    printf(".globl _start\n");
    printf("\n");

    printf("putchar:\n");
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

    current = root;
    while (current) {
        int registers_used = 0;
        codegen(current, &registers_used);
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
