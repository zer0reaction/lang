void print_tokens(const token_t *ts)
{
    assert(TT_COUNT == 21);

    for (u64 i = 0; i < arrlenu(ts); i++) {
        token_t t = ts[i];

        switch (t.type) {
        case TT_IDENT:
            printf("`%s`", t.ident_name);
            break;
        case TT_INT_LITERAL:
            printf("<%d>", t.int_value);
            break;
        case TT_CHAR_LITERAL:
            printf("'%c'", (char)t.int_value);
            break;
        case TT_COLUMN_EQUAL:
            printf("=");
            break;
        case TT_CMP_EQ:
            printf("==");
            break;
        case TT_CMP_NEQ:
            printf("!=");
            break;
        case TT_CMP_LESS:
            printf("<");
            break;
        case TT_CMP_LESS_OR_EQ:
            printf("<=");
            break;
        case TT_CMP_GREATER:
            printf(">");
            break;
        case TT_CMP_GREATER_OR_EQ:
            printf(">=");
            break;
        case TT_PLUS:
            printf("+");
            break;
        case TT_MINUS:
            printf("-");
            break;
        case TT_LET:
            printf("let");
            break;
        case TT_WHILE:
            printf("while");
            break;
        case TT_IF:
            printf("if");
            break;
        case TT_ELSE:
            printf("else");
            break;
        case TT_ROUND_OPEN:
            printf("(");
            break;
        case TT_ROUND_CLOSE:
            printf(")");
            break;
        case TT_CURLY_OPEN:
            printf("{");
            break;
        case TT_CURLY_CLOSE:
            printf("}");
            break;
        default:
            assert(0 && "unexpected token");
        }
        printf(" ");
    }

    printf("\n");
}

void print_ast(const node_t *n)
{
    assert(NT_COUNT == 18);

    if (n == NULL) return;

    switch (n->type) {
    case NT_ASSIGN: {
        printf("(= ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")\n");
    } break;

    case NT_CMP_EQ: {
        printf("(== ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_CMP_NEQ: {
        printf("(!= ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_CMP_LESS: {
        printf("(< ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_CMP_LESS_OR_EQ: {
        printf("(<= ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_CMP_GREATER: {
        printf("(> ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_CMP_GREATER_OR_EQ: {
        printf("(>= ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_VAR: {
        printf("(%s id:%u offset:%d)", table[n->table_id].ident_name, n->table_id,
               table[n->table_id].stack_offset);
    } break;

    case NT_VAR_DECL: {
        printf("(let `%s` id:%u offset:%d)", table[n->table_id].ident_name, n->table_id,
               table[n->table_id].stack_offset);
    } break;

    case NT_WHILE: {
        printf("while ");
        print_ast(n->while_cond);
        print_ast(n->while_body);
    } break;

    case NT_IF: {
        printf("if ");
        print_ast(n->if_cond);
        print_ast(n->if_body);
        if (n->else_body) {
            printf("else ");
            print_ast(n->else_body);
        }
    } break;

    case NT_FUNC_CALL: {
        printf("`%s allign:%d`(", n->func_name, n->allign_sub);
        for (u64 i = 0; i < n->args_count; i++) {
            print_ast(n->args[i]);
        }
        printf(")\n");
    } break;

    case NT_SCOPE: {
        printf("{ id:%u\n", n->scope_id);
        print_ast(n->scope_start);
        printf("}\n");
    } break;

    case NT_INT: {
        printf("<%d>", n->int_value);
    } break;

    case NT_CHAR: {
        printf("'%c'", (char)n->int_value);
    } break;

    case NT_SUM: {
        printf("(+ ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    case NT_SUB: {
        printf("(- ");
        print_ast(n->lval);
        print_ast(n->rval);
        printf(")");
    } break;

    default: {
        assert(0 && "unexpected node");
    } break;
    }

    printf(" ");
    print_ast(n->next);
}
