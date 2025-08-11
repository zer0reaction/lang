#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

/* -------------------------------------------------------------------------------- */
/* External libs                                                                    */
/* -------------------------------------------------------------------------------- */

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

/* -------------------------------------------------------------------------------- */
/* Preprocessor                                                                     */
/* -------------------------------------------------------------------------------- */

#define SCOPE_IDS_BUF_SIZE 32
#define IDENT_NAME_MAX_LEN 32
#define ARENA_REGION_DEFAULT_CAPACITY (4 * 1024)

#define ABSOLUTE(a) ((a) >= 0 ? (a) : -(a))

/* -------------------------------------------------------------------------------- */
/* Global immutable variables                                                       */
/* -------------------------------------------------------------------------------- */

static const char *scratch_4b_registers[] = {
    "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"
};
static const char *scratch_1b_registers[] = {
    "dil", "sil", "dl", "cl", "r8b", "r9b", "r10b", "r11b"
};
#define SCRATCH_REGISTERS_COUNT (sizeof(scratch_4b_registers) / sizeof(*scratch_4b_registers))

static const char *argument_registers[] = {
    "edi", "esi", "edx", "ecx", "r8d", "r9d"
};
#define ARGUMENT_REGISTERS_COUNT (sizeof(argument_registers) / sizeof(*argument_registers))

/* -------------------------------------------------------------------------------- */
/* Integer type defenitions                                                         */
/* -------------------------------------------------------------------------------- */

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef unsigned uint;

/* -------------------------------------------------------------------------------- */
/* Token type                                                                       */
/* -------------------------------------------------------------------------------- */

typedef enum {
    TT_EMPTY = 0,
    TT_IDENT,
    TT_INT_LITERAL,
    TT_CHAR_LITERAL,
    TT_COLUMN_EQUAL,
    TT_CMP_EQ,
    TT_CMP_NEQ,
    TT_CMP_LESS,
    TT_CMP_LESS_OR_EQ,
    TT_CMP_GREATER,
    TT_CMP_GREATER_OR_EQ,
    TT_PLUS,
    TT_MINUS,
    TT_LET,
    TT_WHILE,
    TT_IF,
    TT_ELSE,
    TT_FN,
    TT_ROUND_OPEN,
    TT_ROUND_CLOSE,
    TT_CURLY_OPEN,
    TT_CURLY_CLOSE,
    TT_SEMICOLON,
    TT_COMMA,
    TT_COUNT
} tt_t;

typedef struct {
    tt_t type;
    union {
        char ident_name[IDENT_NAME_MAX_LEN + 1];
        s32  int_value;
    };
} token_t;

typedef struct {
    char *s;
    tt_t  t;
} token_string_t;

/* NOTE: this is here because of the type dependencies */
static token_string_t token_strings[] = {
    { .s = ":=",    .t = TT_COLUMN_EQUAL      },
    { .s = "==",    .t = TT_CMP_EQ            },
    { .s = "!=",    .t = TT_CMP_NEQ           },
    { .s = "<=",    .t = TT_CMP_LESS_OR_EQ    },
    { .s = ">=",    .t = TT_CMP_GREATER_OR_EQ },
    { .s = "let",   .t = TT_LET               },
    { .s = "while", .t = TT_WHILE             },
    { .s = "if",    .t = TT_IF                },
    { .s = "else",  .t = TT_ELSE              },
    { .s = "fn",    .t = TT_FN                },
    { .s = "+",     .t = TT_PLUS              },
    { .s = "-",     .t = TT_MINUS             },
    { .s = "<",     .t = TT_CMP_LESS          },
    { .s = ">",     .t = TT_CMP_GREATER       },
    { .s = "(",     .t = TT_ROUND_OPEN        },
    { .s = ")",     .t = TT_ROUND_CLOSE       },
    { .s = "{",     .t = TT_CURLY_OPEN        },
    { .s = "}",     .t = TT_CURLY_CLOSE       },
    { .s = ";",     .t = TT_SEMICOLON         },
    { .s = ",",     .t = TT_COMMA             }
};
#define TOKEN_STRINGS_COUNT (sizeof(token_strings) / sizeof(*token_strings))

/* -------------------------------------------------------------------------------- */
/* Symbol type                                                                      */
/* -------------------------------------------------------------------------------- */

typedef struct {
    char ident_name[IDENT_NAME_MAX_LEN + 1];
    s32  stack_offset; /* negative value */
    u32  scope_id;
} symbol_t;

/* -------------------------------------------------------------------------------- */
/* Node type                                                                        */
/* -------------------------------------------------------------------------------- */

typedef enum {
    NT_EMPTY = 0,
    NT_ASSIGN,
    NT_CMP_EQ,
    NT_CMP_NEQ,
    NT_CMP_LESS,
    NT_CMP_LESS_OR_EQ,
    NT_CMP_GREATER,
    NT_CMP_GREATER_OR_EQ,
    NT_VAR,
    NT_VAR_DECL,
    NT_WHILE,
    NT_IF,
    NT_FUNC_CALL,
    NT_FUNC_DECL,
    NT_SCOPE,
    NT_INT,
    NT_CHAR,
    NT_SUM,
    NT_SUB,
    NT_COUNT
} nt_t;

typedef struct node_t node_t;
struct node_t {
    nt_t    type;
    node_t *next;

    union {
        /* integer literal */
        s32  int_value;

        /* variable */
        struct {
            u32  table_id;
            char var_name[IDENT_NAME_MAX_LEN + 1];
        };

        /* while */
        struct {
            node_t *while_cond;
            node_t *while_body;
        };

        /* if */
        struct {
            node_t *if_cond;
            node_t *if_body;
            node_t *else_body;
        };

        /* scope */
        struct {
            u32     scope_id;
            node_t *scope_start;
        };

        /* binary operator */
        struct {
            node_t *lval;
            node_t *rval;
        };

        /* function defenition/call */
        struct {
            char    func_name[IDENT_NAME_MAX_LEN + 1];
            node_t *args[ARGUMENT_REGISTERS_COUNT];
            node_t *func_body;
            u8      args_count;
            u8      allign_sub; /* positive value, subtracted from rsp before call */
        };
    };
};

/* -------------------------------------------------------------------------------- */
/* Storage type                                                                     */
/* -------------------------------------------------------------------------------- */

typedef enum {
    ST_NONE,
    ST_REGISTER,
    ST_STACK,
    ST_IMMEDIATE
} st_t;

typedef struct {
    st_t type;
    bool is_intermediate; /* currently only register values */
    union {
        u32 table_id;
        u8  register_id;
        s32 int_value;
    };
} storage_t;

/* -------------------------------------------------------------------------------- */
/* Arena type                                                                       */
/* -------------------------------------------------------------------------------- */

typedef struct arena_region_t arena_region_t;

struct arena_region_t {
    u64             used;
    u64             capacity;
    arena_region_t *next;
    u8              data[];
};

typedef struct {
    arena_region_t *head;
    arena_region_t *tail;
} arena_t;

/* -------------------------------------------------------------------------------- */
/* Operator priority type                                                           */
/* -------------------------------------------------------------------------------- */

typedef enum {
    OP_PRIORITY_UNARY          = 0,
    OP_PRIORITY_BINARY_ADD_SUB = 1,
    OP_PRIORITY_BINARY_COMP    = 2,
    OP_PRIORITY_BINARY_ASSIGN  = 3
} op_priority_t;

/* -------------------------------------------------------------------------------- */
/* Global mutable variables                                                         */
/* -------------------------------------------------------------------------------- */

static symbol_t *table = NULL;
static bool registers_in_use[SCRATCH_REGISTERS_COUNT] = { 0 };

/* -------------------------------------------------------------------------------- */
/* Code                                                                             */
/* -------------------------------------------------------------------------------- */

#include "testing.c"

arena_region_t *arena__region_create(u64 n) {
    arena_region_t *r = NULL;

    if (n > ARENA_REGION_DEFAULT_CAPACITY) {
        r = malloc(sizeof *r + n);
    } else {
        r = malloc(sizeof *r + ARENA_REGION_DEFAULT_CAPACITY);
    }
    memset(r, 0, sizeof *r);

    return r;
}

void *arena_alloc(arena_t *a, u64 n) {
    void *ptr = NULL;

    if (!a->tail) {
        arena_region_t *r = arena__region_create(n);
        a->head = a->tail = r;
        return r->data;
    } else if (a->tail->used + n > a->tail->capacity) {
        arena_region_t *r = arena__region_create(n);
        a->tail->next = r;
        a->tail = r;
        return r->data;
    }

    ptr = &a->tail->data[a->tail->used];
    a->tail->used += n;
    return ptr;
}

void *arena_alloc_initz(arena_t *a, u64 n) {
    void *ptr = arena_alloc(a, n);
    memset(ptr, 0, n);
    return ptr;
}

void arena_free(arena_t *a) {
    arena_region_t *current = a->head;

    while (current) {
        arena_region_t *next = current->next;
        free(current);
        current = next;
    }
}

token_t *tokenize(const char *s, u64 len) {
    u64 i = 0;
    token_t *ts = NULL;

    assert(TT_COUNT == 24);

    while (i < len) {
        /* blank characters */
        if (s[i] == ' ' || s[i] == '\t' || s[i] == '\n') {
            i += 1;
            continue;
        }

        /* pre-defined tokens */
        bool match = false;
        for (u64 j = 0; j < TOKEN_STRINGS_COUNT; j++) {
            token_string_t current = token_strings[j];
            if (len - i >= strlen(current.s) && strncmp(&s[i], current.s, strlen(current.s)) == 0) {
                token_t t = {0};
                t.type = current.t;
                arrput(ts, t);
                i += strlen(current.s);
                match = true;
                break;
            }
        }
        if (match) continue;

        /* character literal */
        /* TODO: breaks on something like '0 ' */
        if (s[i] == '\'') {
            assert(len - i >= 3);
            i += 1;

            token_t t = {0};
            t.type = TT_CHAR_LITERAL;

            if (s[i] == '\\') {
                assert(len - i >= 3 && s[i + 2] == '\'');
                i += 1;
                /* TODO: add more escape characters */
                switch (s[i]) {
                case 't':
                    t.int_value = '\t';
                    break;
                case 'n':
                    t.int_value = '\n';
                    break;
                case '\\':
                    t.int_value = '\\';
                    break;
                case '\'':
                    t.int_value = '\'';
                    break;
                default:
                    assert(0 && "unrecognized escape sequence");
                }
                i += 2;
            } else {
                t.int_value = (s32)s[i];
                i += 2;
            }

            arrput(ts, t);
            continue;
        }

        /* int literal */
        if (s[i] >= '0' && s[i] <= '9') {
            s32 value = 0;
            while (i < len && s[i] >= '0' && s[i] <= '9') {
                value *= 10;
                value += s[i] - '0';
                i += 1;
            }

            token_t t = {0};
            t.type = TT_INT_LITERAL;
            t.int_value = value;

            arrput(ts, t);
            continue;
        }

        /* identifier */
        if ((s[i] >= 'a' && s[i] <= 'z') || s[i] == '_') {
            token_t t = {0};
            t.type = TT_IDENT;

            u32 name_len = 0;
            while (len - i > 0 && ((s[i] >= 'a' && s[i] <= 'z') || s[i] == '_')) {
                assert(name_len < IDENT_NAME_MAX_LEN);
                t.ident_name[name_len++] = s[i++];
            }

            arrput(ts, t);
            continue;
        }

        assert(0 && "unexpected character");
    }

    return ts;
}

op_priority_t operator_get_priority(token_t t) {
    switch (t.type) {
        /* 0) left to right */
    case TT_LET:
        return OP_PRIORITY_UNARY;

        /* 1) left to right */
    case TT_PLUS:
    case TT_MINUS:
        return OP_PRIORITY_BINARY_ADD_SUB;

        /* 2) left to right */
    case TT_CMP_LESS:
    case TT_CMP_GREATER:
    case TT_CMP_LESS_OR_EQ:
    case TT_CMP_GREATER_OR_EQ:
    case TT_CMP_EQ:
    case TT_CMP_NEQ:
        return OP_PRIORITY_BINARY_COMP;

        /* 3) right to left */
    case TT_COLUMN_EQUAL:
        return OP_PRIORITY_BINARY_ASSIGN;

        /* -1 on error */
    default:
        return -1;
    }
}

uint token_length_to_type(const token_t *ts, uint start, tt_t type) {
    uint len = 0;

    while (ts[start + len].type != type) {
        assert(start + len < arrlenu(ts));
        len++;
    }

    return len;
}

node_t *parse_statement(const token_t *ts, uint start, uint len, arena_t *a) {
    if (len == 1) {
        node_t *n = arena_alloc_initz(a, sizeof *n);

        switch (ts[start].type) {
        case TT_CHAR_LITERAL:
        case TT_INT_LITERAL:
            n->type = NT_INT;
            n->int_value = ts[start].int_value;
            break;

        case TT_IDENT:
            n->type = NT_VAR;
            strcpy(n->var_name, ts[start].ident_name);
            break;

        default:
            assert(0 && "unexpected token in statement (parsing 1 token)");
        }

        return n;
    } else if (len == 2) {
        node_t *n = arena_alloc_initz(a, sizeof *n);

        switch (ts[start].type) {
        case TT_LET:
            assert(ts[start + 1].type == TT_IDENT);
            n->type = NT_VAR_DECL;
            strcpy(n->var_name, ts[start + 1].ident_name);
            break;

        default:
            assert(0 && "unexpected token in statement (parsing 2 tokens)");
        }

        return n;
    } else if (len >= 3 && ts[start + 1].type == TT_ROUND_OPEN) {
        node_t *n = arena_alloc_initz(a, sizeof *n);

        n->type = NT_FUNC_CALL;
        strcpy(n->func_name, ts[start].ident_name);
        start += 2;

        while (ts[start].type != TT_ROUND_CLOSE) {
            uint arg_len = 0;
            node_t *arg = NULL;

            assert(arrlenu(ts) - start > 1);
            assert(n->args_count < ARGUMENT_REGISTERS_COUNT);

            while (ts[start + arg_len].type != TT_COMMA &&
                   ts[start + arg_len].type != TT_ROUND_CLOSE) {

                arg_len += 1;
            }

            arg = parse_statement(ts, start, arg_len, a);

            if (ts[start + arg_len].type == TT_COMMA) {
                start += arg_len + 1;
            } else {
                start += arg_len;
            }

            n->args[n->args_count++] = arg;
        }

        return n;
    }

    {
        u64 i = 0;
        int lowest_priority = -1;
        uint op_ind;
        uint lhs_start, lhs_len;
        uint rhs_start, rhs_len;
        node_t *n = arena_alloc_initz(a, sizeof *n);

        for (i = start; i < start + len; i++) {
            int priority = operator_get_priority(ts[i]);
            if (priority >= lowest_priority) {
                lowest_priority = priority;
                op_ind = i;     /* for left-to-right evaluation */
            }
        }
        assert(lowest_priority >= 0);

        /* assuming that all the operations are binary at this point */

        /* right now ':=' is the only operator with right-to-left evaluation */
        if (lowest_priority == OP_PRIORITY_BINARY_ASSIGN) {
            for (i = start; i < start + len; i++) {
                if ((int)operator_get_priority(ts[i]) == lowest_priority) {
                    op_ind = i;
                    break;
                }
            }
        }

        lhs_start = start;
        lhs_len = op_ind - start;
        rhs_start = op_ind + 1;
        rhs_len = (start + len) - (op_ind + 1);

        switch (ts[op_ind].type) {
        case TT_COLUMN_EQUAL:
            n->type = NT_ASSIGN;
            break;

        case TT_CMP_EQ:
            n->type = NT_CMP_EQ;
            break;

        case TT_CMP_NEQ:
            n->type = NT_CMP_NEQ;
            break;

        case TT_CMP_LESS:
            n->type = NT_CMP_LESS;
            break;

        case TT_CMP_LESS_OR_EQ:
            n->type = NT_CMP_LESS_OR_EQ;
            break;

        case TT_CMP_GREATER:
            n->type = NT_CMP_GREATER;
            break;

        case TT_CMP_GREATER_OR_EQ:
            n->type = NT_CMP_GREATER_OR_EQ;
            break;

        case TT_PLUS:
            n->type = NT_SUM;
            break;

        case TT_MINUS:
            n->type = NT_SUB;
            break;

        default:
            assert(0 && "invalid binary operation type");
        }

        n->lval = parse_statement(ts, lhs_start, lhs_len, a);
        n->rval = parse_statement(ts, rhs_start, rhs_len, a);

        return n;
    }
}

node_t *parse(const token_t *ts, u32 *eaten, arena_t *a) {
    assert(TT_COUNT == 24);
    assert(NT_COUNT == 19);

    assert(*eaten < arrlenu(ts));

    node_t *n = arena_alloc_initz(a, sizeof(*n));

    switch (ts[*eaten].type) {
    case TT_WHILE: {
        uint len = 0;

        n->type = NT_WHILE;
        *eaten += 1;
        len = token_length_to_type(ts, *eaten, TT_CURLY_OPEN);

        n->while_cond = parse_statement(ts, *eaten, len, a);
        *eaten += len;
        n->while_body = parse(ts, eaten, a);
    } break;

    case TT_IF: {
        uint len = 0;

        n->type = NT_IF;
        *eaten += 1;
        len = token_length_to_type(ts, *eaten, TT_CURLY_OPEN);

        n->if_cond = parse_statement(ts, *eaten, len, a);
        *eaten += len;
        n->if_body = parse(ts, eaten, a);

        if (*eaten < arrlenu(ts) && ts[*eaten].type == TT_ELSE) {
            *eaten += 1;
            n->else_body = parse(ts, eaten, a);
        }
    } break;

    case TT_FN: {
        assert(arrlenu(ts) - *eaten >= 6);

        n->type = NT_FUNC_DECL;
        *eaten += 1;

        assert(ts[*eaten].type == TT_IDENT);
        strcpy(n->func_name, ts[*eaten].ident_name);
        *eaten += 1;

        assert(ts[*eaten].type == TT_ROUND_OPEN);
        *eaten += 1;

        while (ts[*eaten].type != TT_ROUND_CLOSE) {
            uint arg_len = 0;
            node_t *arg = NULL;

            assert(arrlenu(ts) - *eaten > 1);
            assert(n->args_count < ARGUMENT_REGISTERS_COUNT);

            while (ts[*eaten + arg_len].type != TT_COMMA &&
                   ts[*eaten + arg_len].type != TT_ROUND_CLOSE) {

                arg_len += 1;
            }

            arg = parse_statement(ts, *eaten, arg_len, a);
            assert(arg->type == NT_VAR);

            if (ts[*eaten + arg_len].type == TT_COMMA) {
                *eaten += arg_len + 1;
            } else {
                *eaten += arg_len;
            }

            n->args[n->args_count++] = arg;
        }
        *eaten += 1;

        n->func_body = parse(ts, eaten, a);
        assert(n->func_body->type == NT_SCOPE);
    } break;

    case TT_CURLY_OPEN: {
        n->type = NT_SCOPE;
        *eaten += 1;
        node_t *current = NULL;
        while (ts[*eaten].type != TT_CURLY_CLOSE) {
            assert(arrlenu(ts) - *eaten > 1);
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
        uint statement_len = token_length_to_type(ts, *eaten, TT_SEMICOLON);
        n = parse_statement(ts, *eaten, statement_len, a);
        *eaten += statement_len + 1;
    } break;
    }

    return n;
}

void pass_set_scope_ids(node_t *n, u32 *scope_count) {
    assert(NT_COUNT == 19);

    if (!n) return;

    switch (n->type) {
    case NT_SCOPE: {
        n->scope_id = ++(*scope_count);
        pass_set_scope_ids(n->scope_start, scope_count);
    } break;

    case NT_WHILE: {
        pass_set_scope_ids(n->while_body, scope_count);
    } break;

    case NT_IF: {
        pass_set_scope_ids(n->if_body, scope_count);
        pass_set_scope_ids(n->else_body, scope_count);
    } break;

    case NT_FUNC_DECL: {
        pass_set_scope_ids(n->func_body, scope_count);
    } break;

    default:
        /* no need to call on other things */
        break;
    }

    pass_set_scope_ids(n->next, scope_count);
}

void pass_set_table_ids(node_t *n, s32 *stack_offset, const u32 scope_ids[], u32 size, arena_t *a) {
    assert(NT_COUNT == 19);

    if (!n) return;

    switch (n->type) {
    case NT_VAR_DECL: {
        assert(size >= 1);
        bool in_current_scope = false;
        u32 current_scope_id = scope_ids[size - 1];

        for (u64 i = 0; i < arrlenu(table); i++) {
            if (table[i].scope_id == current_scope_id && n->var_name == table[i].ident_name) {
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
        n->table_id = arrlenu(table) - 1;
    } break;

    case NT_VAR: {
        bool visible = false;
        u32 table_id = 0;
        for (u64 i = 0; i < arrlenu(table); i++) {
            if (strcmp(n->var_name, table[i].ident_name) == 0) {
                for (s64 j = size - 1; j >= 0; j--) {
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
        u32 new_scope_ids[SCOPE_IDS_BUF_SIZE] = {0};
        for (u64 i = 0; i < size; i++) {
            new_scope_ids[i] = scope_ids[i];
        }
        new_scope_ids[size] = n->scope_id;
        pass_set_table_ids(n->scope_start, stack_offset, new_scope_ids, size + 1, a);
    } break;

    case NT_SUM:
    case NT_SUB:
    case NT_CMP_EQ:
    case NT_CMP_NEQ:
    case NT_CMP_LESS:
    case NT_CMP_LESS_OR_EQ:
    case NT_CMP_GREATER:
    case NT_CMP_GREATER_OR_EQ:
    case NT_ASSIGN: {
        pass_set_table_ids(n->lval, stack_offset, scope_ids, size, a);
        pass_set_table_ids(n->rval, stack_offset, scope_ids, size, a);
    } break;

    case NT_WHILE: {
        pass_set_table_ids(n->while_cond, stack_offset, scope_ids, size, a);
        pass_set_table_ids(n->while_body, stack_offset, scope_ids, size, a);
    } break;

    case NT_IF: {
        pass_set_table_ids(n->if_cond, stack_offset, scope_ids, size, a);
        pass_set_table_ids(n->if_body, stack_offset, scope_ids, size, a);
        pass_set_table_ids(n->else_body, stack_offset, scope_ids, size, a);
    } break;

    case NT_FUNC_CALL: {
        /* TODO: refactor, lame */
        n->allign_sub = ABSOLUTE(*stack_offset) + (16 - ABSOLUTE(*stack_offset) % 16) % 16;
        for (u64 i = 0; i < n->args_count; i++) {
            pass_set_table_ids(n->args[i], stack_offset, scope_ids, size, a);
        }
    } break;

    case NT_FUNC_DECL: {
        for (s64 i = n->args_count - 1; i >= 0; i--) {
            node_t *var_decl = arena_alloc_initz(a, sizeof(*var_decl));

            var_decl->type = NT_VAR_DECL;
            strcpy(var_decl->var_name, n->args[i]->var_name);

            var_decl->next = n->func_body->scope_start;
            n->func_body->scope_start = var_decl;
        }

        s32 func_stack = 0;
        pass_set_table_ids(n->func_body, &func_stack, scope_ids, size, a);
    } break;

    default:
        break;
    }

    pass_set_table_ids(n->next, stack_offset, scope_ids, size, a);
}

uint register_reserve(void) {
    uint i;

    for (i = 0; i < SCRATCH_REGISTERS_COUNT; i++) {
        if (!registers_in_use[i]) {
            registers_in_use[i] = true;
            return i;
        }
    }

    assert(0 && "reserve_register: not enough free registers");
}

void register_free(uint id) {
    registers_in_use[id] = false;
}

void register_free_all(void) {
    uint i;

    for (i = 0; i < SCRATCH_REGISTERS_COUNT; i++) {
        registers_in_use[i] = false;
    }
}

void storage_unwrap(storage_t st) {
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
storage_t storage_move_to_register(storage_t st) {
    switch (st.type) {
    case ST_REGISTER: {
        return st;
    } break;

    case ST_IMMEDIATE: {
        u8 register_id = register_reserve();

        printf("\tmovl\t$%d, %%%s\n",
               st.int_value,
               scratch_4b_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER,
                            .is_intermediate = true,
                            .register_id = register_id };
    } break;

    case ST_STACK: {
        u8 register_id = register_reserve();

        printf("\tmovl\t%d(%%rbp), %%%s\n",
               table[st.table_id].stack_offset,
               scratch_4b_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER,
                            .is_intermediate = true,
                            .register_id = register_id };
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
storage_t storage_make_mutable(storage_t st) {
    switch (st.type) {
    case ST_IMMEDIATE: {
        u8 register_id = register_reserve();

        printf("\tmovl\t$%d, %%%s\n",
               st.int_value,
               scratch_4b_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER,
                            .is_intermediate = true,
                            .register_id = register_id };
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

void storage_free_intermediate(storage_t st) {
    if (st.is_intermediate) {
        register_free(st.register_id);
    }
}

storage_t codegen(const node_t *n) {
    static uint local_label_count = 0;

    assert(NT_COUNT == 19);

    switch (n->type) {
    case NT_VAR_DECL:
    case NT_VAR: {
        return (storage_t){ .table_id = n->table_id, .type = ST_STACK };
    } break;

    case NT_CHAR:
    case NT_INT: {
        return (storage_t){ .type = ST_IMMEDIATE, .int_value = n->int_value };
    } break;

    case NT_ASSIGN: {
        storage_t lval = codegen(n->lval);
        storage_t rval_init = codegen(n->rval);
        assert(lval.type == ST_STACK && rval_init.type != ST_NONE);

        storage_t rval_proc = {0};
        if (rval_init.type == ST_STACK) {
            rval_proc = storage_move_to_register(rval_init);
        } else {
            rval_proc = rval_init;
        }

        printf("\tmovl\t");
        storage_unwrap(rval_proc);
        printf(", ");
        storage_unwrap(lval);
        printf("\n");

        storage_free_intermediate(rval_proc);
        return rval_proc;
    } break;

    case NT_CMP_EQ: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsete\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_CMP_NEQ: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsetne\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_CMP_LESS: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsetl\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_CMP_LESS_OR_EQ: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsetle\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_CMP_GREATER: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsetg\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_CMP_GREATER_OR_EQ: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tcmpl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");
        printf("\tsetge\t%%%s\n", scratch_1b_registers[lval_reg.register_id]);
        printf("\tandl\t$0xFF, ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_SUM: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\taddl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_SUB: {
        storage_t lval_init = codegen(n->lval);
        storage_t rval = codegen(n->rval);
        assert(lval_init.type != ST_NONE && rval.type != ST_NONE);

        storage_t lval_reg = storage_move_to_register(lval_init);

        printf("\tsubl\t");
        storage_unwrap(rval);
        printf(", ");
        storage_unwrap(lval_reg);
        printf("\n");

        storage_free_intermediate(rval);
        return lval_reg;
    } break;

    case NT_FUNC_CALL: {
        u8 storage_count = n->args_count;

        /* TODO: registers can be overriden, fix! */
        for (u64 i = 0; i < storage_count; i++) {
            storage_t st = codegen(n->args[i]);
            assert(st.type != ST_NONE);

            printf("\tmovl\t");
            storage_unwrap(st);
            printf(", %%%s\n", argument_registers[i]);

            storage_free_intermediate(st);
        }

        if (n->allign_sub > 0) {
            printf("\tsubq\t$%d, %%rsp\n", n->allign_sub);
        }
        printf("\tcall\t%s\n", n->func_name);
        if (n->allign_sub > 0) {
            printf("\taddq\t$%d, %%rsp\n", n->allign_sub); /* TODO: refactor, lame */
        }

        /* TODO: currently recursive function calls can modify
           the registers of the previous calls, too bad, FIX! */
        u8 register_id = register_reserve();
        printf("\tmovl\t%%eax, %%%s\n", scratch_4b_registers[register_id]);

        return (storage_t){ .type = ST_REGISTER,
                            .is_intermediate = true,
                            .register_id = register_id };
    } break;

    case NT_FUNC_DECL: {
        /* variable nodes in the args are just names, there are no ids assigned to them */
        /* variable declaration nodes are pushed at the start of the function body (scope) */

        printf("%s:\n", n->func_name);
        printf("\tpushq\t%%rbp\n");
        printf("\tmovq\t%%rsp, %%rbp\n");

        node_t *current = n->func_body->scope_start;
        for (u64 i = 0; i < n->args_count; i++) {
            storage_t st;

            printf("\tmovl\t%%%s, ", argument_registers[i]);

            st = codegen(current);
            storage_unwrap(st);
            storage_free_intermediate(st);

            printf("\n");
            current = current->next;
        }

        storage_t return_val = { .type = ST_NONE };

        while (current) {
            if (!current->next) {
                return_val = codegen(current);
                if (return_val.type != ST_NONE) {
                    printf("\tmovl\t");
                    storage_unwrap(return_val);
                    printf(", %%eax\n");
                    storage_free_intermediate(return_val);
                }
            } else {
                storage_free_intermediate(codegen(current));
            }
            current = current->next;
        }

        printf("\tleave\n");
        printf("\tret\n");
        printf("\n");

        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_SCOPE: {
        register_free_all();
        node_t *current = n->scope_start;
        while (current) {
            storage_free_intermediate(codegen(current));
            current = current->next;
        }
        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_WHILE: {
        u32 ident = local_label_count++;

        printf(".while_start_%u:\n", ident);

        storage_t cond_init = codegen(n->while_cond);
        assert(cond_init.type != ST_NONE);
        storage_t cond_mut = storage_make_mutable(cond_init);

        printf("\tcmpl\t$0, ");
        storage_unwrap(cond_mut);
        printf("\n");
        printf("\tjz\t.while_end_%u\n", ident);

        storage_free_intermediate(codegen(n->while_body));

        printf("\tjmp\t.while_start_%u\n", ident);
        printf(".while_end_%u:\n", ident);

        storage_free_intermediate(cond_mut);
        return (storage_t){ .type = ST_NONE };
    } break;

    case NT_IF: {
        u32 ident = local_label_count++;
        bool has_else = (n->else_body != NULL);

        storage_t cond_init = codegen(n->if_cond);
        assert(cond_init.type != ST_NONE);
        storage_t cond_mut = storage_make_mutable(cond_init);

        printf("\tcmpl\t$0, ");
        storage_unwrap(cond_mut);
        printf("\n");

        if (has_else) {
            printf("\tjz\t.else_start_%d\n", ident);
            storage_free_intermediate(codegen(n->if_body));
            printf("\tjmp\t.if_end_%d\n", ident);

            printf(".else_start_%d:\n", ident);
            storage_free_intermediate(codegen(n->else_body));
        } else {
            printf("\tjz\t.if_end_%d\n", ident);
            storage_free_intermediate(codegen(n->if_body));
        }

        printf(".if_end_%d:\n", ident);

        storage_free_intermediate(cond_mut);
        return (storage_t){ .type = ST_NONE };
    } break;

    default: {
        assert(0 && "unexpected node");
    } break;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("incorrect number of arguments\n");
        return 1;
    }

    arena_t a = {0};

    FILE *fp = fopen(argv[1], "r");
    if (!fp) {
        printf("can't open file %s\n", argv[1]);
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    u64 file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *contents = malloc(file_size + 1);
    assert(contents && "malloc failed");

    fread(contents, 1, file_size, fp);
    contents[file_size] = '\0';
    fclose(fp);

    token_t *ts = tokenize(contents, strlen(contents));
    free(contents);

    u32 eaten = 0;
    node_t *functions = NULL;
    node_t *root = NULL;
    node_t *current_glob = NULL;
    node_t *current_func = NULL;

    /* TODO: refactor */
    while (eaten < arrlenu(ts)) {
        node_t *n = parse(ts, &eaten, &a);

        if (n->type == NT_FUNC_DECL) {
            if (!functions) {
                functions = n;
                current_func = functions;
            } else {
                current_func->next = n;
                current_func = current_func->next;
            }
        } else {
            if (!root) {
                root = n;
                current_glob = root;
            } else {
                current_glob->next = n;
                current_glob = current_glob->next;
            }
        }
    }

    u32 scope_count = 0;
    pass_set_scope_ids(root, &scope_count);
    pass_set_scope_ids(functions, &scope_count);

    s32 stack_offset = 0;
    u32 scope_ids[SCOPE_IDS_BUF_SIZE] = {0};
    pass_set_table_ids(root, &stack_offset, scope_ids, 1, &a);
    pass_set_table_ids(functions, &stack_offset, scope_ids, 1, &a);

    /*
    print_tokens(ts);
    printf("\n");
    print_ast(root);
    printf("\n\n");
    */

    node_t *current_node = NULL;

    printf(".section .text\n");
    printf(".globl _start\n");
    printf("\n");

    current_node = functions;
    while (current_node) {
        register_free_all();
        storage_free_intermediate(codegen(current_node));
        current_node = current_node->next;
    }

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

    current_node = root;
    while (current_node) {
        register_free_all();
        storage_free_intermediate(codegen(current_node));
        current_node = current_node->next;
    }

    printf("\tmovq\t$60, %%rax\n");
    printf("\tmovq\t$0, %%rdi\n");
    printf("\tsyscall\n");

    arena_free(&a);
    arrfree(ts);
    arrfree(table);
    return 0;
}
