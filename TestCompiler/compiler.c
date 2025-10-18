// TestCompilerPointers.c
#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// ---------- TOKENIZER ----------

typedef enum {
    TK_EOF,
    TK_INT,
    TK_RETURN,
    TK_IDENT,
    TK_NUMBER,
    TK_ASSIGN,
    TK_PLUS,
    TK_SEMI,
    TK_LPAREN,
    TK_RPAREN,
    TK_LBRACE,
    TK_RBRACE,
    TK_STAR,    // '*'
    TK_AMP      // '&'
} TokenKind;

typedef struct {
    TokenKind kind;
    char* str;
    long val;
} Token;

#define MAXTOK 8192
Token tokens[MAXTOK];
int tok_count = 0, tok_pos = 0;
char* src;
int srcpos = 0;

void add_token(Token t) {
    if (tok_count >= MAXTOK) { fprintf(stderr, "too many tokens\n"); exit(1); }
    tokens[tok_count++] = t;
}

Token* peek_tok() { return &tokens[tok_pos]; }
Token* consume_tok() { return &tokens[tok_pos++]; }
int match(TokenKind k) { return tokens[tok_pos].kind == k; }
void expect(TokenKind k) {
    if (!match(k)) {
        fprintf(stderr, "expected token kind %d got %d\n", k, tokens[tok_pos].kind);
        exit(1);
    }
    tok_pos++;
}

// strdup fallback
static char* my_strdup(const char* s) {
    size_t len = strlen(s) + 1;
    char* d = malloc(len);
    memcpy(d, s, len);
    return d;
}

void lex_number() {
    int start = srcpos;
    while (isdigit((unsigned char)src[srcpos])) srcpos++;
    int len = srcpos - start;
    char tmp[64];
    if (len >= (int)sizeof(tmp)) { fprintf(stderr, "number too long\n"); exit(1); }
    memcpy(tmp, src + start, len);
    tmp[len] = 0;
    Token t = { TK_NUMBER, NULL, strtol(tmp, NULL, 10) };
    add_token(t);
}

void lex_ident_or_kw() {
    int start = srcpos;
    while (isalnum((unsigned char)src[srcpos]) || src[srcpos] == '_') srcpos++;
    int len = srcpos - start;
    char* id = malloc(len + 1);
    memcpy(id, src + start, len);
    id[len] = 0;

    Token t = { 0 };
    if (!strcmp(id, "int")) t.kind = TK_INT;
    else if (!strcmp(id, "return")) t.kind = TK_RETURN;
    else { t.kind = TK_IDENT; t.str = id; }
    add_token(t);
}

void tokenize() {
    while (src[srcpos]) {
        char c = src[srcpos];

        // whitespace
        if (isspace((unsigned char)c)) { srcpos++; continue; }

        // line comment //
        if (c == '/' && src[srcpos + 1] == '/') {
            srcpos += 2;
            while (src[srcpos] && src[srcpos] != '\n') srcpos++;
            continue;
        }

        if (isalpha((unsigned char)c) || c == '_') { lex_ident_or_kw(); continue; }
        if (isdigit((unsigned char)c)) { lex_number(); continue; }

        Token t = { 0 };
        switch (c) {
        case '=': t.kind = TK_ASSIGN; break;
        case '+': t.kind = TK_PLUS; break;
        case ';': t.kind = TK_SEMI; break;
        case '(': t.kind = TK_LPAREN; break;
        case ')': t.kind = TK_RPAREN; break;
        case '{': t.kind = TK_LBRACE; break;
        case '}': t.kind = TK_RBRACE; break;
        case '*': t.kind = TK_STAR; break;
        case '&': t.kind = TK_AMP; break;
        default:
            fprintf(stderr, "unexpected char '%c'\n", c);
            exit(1);
        }
        srcpos++;
        add_token(t);
    }
    Token end = { TK_EOF, NULL, 0 };
    add_token(end);
}

// ---------- SYMBOL TABLE ----------

typedef struct { char* name; int slot; int is_ptr; } Var;
Var vars[256];
int var_count = 0;

int find_var(const char* name) {
    for (int i = 0; i < var_count; i++)
        if (strcmp(vars[i].name, name) == 0)
            return vars[i].slot;
    return -1;
}

int add_var(const char* name, int is_ptr) {
    int idx = var_count++;
    vars[idx].name = my_strdup(name);
    vars[idx].slot = idx;
    vars[idx].is_ptr = is_ptr;
    return idx;
}

// ---------- EXPRESSIONS ----------

typedef enum { EXPR_NUMBER, EXPR_VAR, EXPR_ADD, EXPR_ADDR, EXPR_DEREF } ExprKind;

typedef struct Expr {
    ExprKind kind;
    long val;     // for number
    char* name;   // for var / addr / deref
    struct Expr* left;
    struct Expr* right;
} Expr;

Expr* parse_expr(); // forward

Expr* parse_primary() {
    Token* t = peek_tok();

    // address-of: &ident
    if (match(TK_AMP)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after &\n"); exit(1); }
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_ADDR;
        e->name = my_strdup(id->str);
        return e;
    }

    // deref as expression: *ident  (e.g. return *p;)
    if (match(TK_STAR)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_DEREF;
        e->name = my_strdup(id->str);
        return e;
    }

    if (match(TK_NUMBER)) {
        Token* n = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_NUMBER;
        e->val = n->val;
        return e;
    }

    if (match(TK_IDENT)) {
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_VAR;
        e->name = my_strdup(id->str);
        return e;
    }

    fprintf(stderr, "unexpected token in primary\n");
    exit(1);
}

Expr* parse_expr() {
    Expr* left = parse_primary();
    while (match(TK_PLUS)) {
        consume_tok();
        Expr* right = parse_primary();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_ADD;
        e->left = left;
        e->right = right;
        left = e;
    }
    return left;
}

// ---------- CODEGEN ----------

FILE* out;

void gen_expr(Expr* e) {
    switch (e->kind) {
    case EXPR_NUMBER:
        fprintf(out, "    mov eax, %ld\n", e->val);
        break;

    case EXPR_VAR: {
        int slot = find_var(e->name);
        if (slot == -1) { fprintf(stderr, "unknown variable '%s'\n", e->name); exit(1); }
        fprintf(out, "    mov eax, [var%d]\n", slot);
        break;
    }

    case EXPR_ADDR: {
        int slot = find_var(e->name);
        if (slot == -1) { fprintf(stderr, "unknown variable '%s' for &\n", e->name); exit(1); }
        // load address of var slot (label)
        fprintf(out, "    lea eax, [var%d]\n", slot);
        break;
    }

    case EXPR_DEREF: {
        // *ident  -> load pointer stored in ident, then load value at that pointer
        int slot = find_var(e->name);
        if (slot == -1) { fprintf(stderr, "unknown variable '%s' for *\n", e->name); exit(1); }
        fprintf(out, "    mov eax, [var%d]\n", slot); // eax = pointer
        fprintf(out, "    mov eax, [eax]\n");       // eax = *pointer
        break;
    }

    case EXPR_ADD:
        // evaluate left -> eax, push, evaluate right -> eax, pop ebx, add eax, ebx
        gen_expr(e->left);
        fprintf(out, "    push eax\n");
        gen_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    add eax, ebx\n");
        break;
    }
}

// ---------- PARSER & EMIT (handles declarations, deref-assignment, return) ----------

void parse_function_and_emit() {
    expect(TK_INT);
    expect(TK_IDENT); // func name
    expect(TK_LPAREN);
    expect(TK_RPAREN);
    expect(TK_LBRACE);

    while (!match(TK_RBRACE) && !match(TK_EOF)) {
        // declaration: int [*] ident = expr;
        if (match(TK_INT)) {
            expect(TK_INT);
            int is_ptr = 0;
            if (match(TK_STAR)) { consume_tok(); is_ptr = 1; } // int* p
            Token* vartok = peek_tok();
            expect(TK_IDENT);
            int slot = add_var(vartok->str, is_ptr);
            expect(TK_ASSIGN);
            Expr* rhs = parse_expr();
            expect(TK_SEMI);

            // generate code to evaluate rhs into eax
            gen_expr(rhs);

            // store eax into var slot (var holds either integer value or pointer value)
            fprintf(out, "    mov [var%d], eax ; %s%s\n", slot, is_ptr ? "int* " : "int ", vartok->str);
            continue;
        }

        // pointer-store: *ident = expr;
        if (match(TK_STAR)) {
            consume_tok();
            Token* id = peek_tok();
            expect(TK_IDENT);
            int ptrslot = find_var(id->str);
            if (ptrslot == -1) { fprintf(stderr, "unknown variable '%s' in *assignment\n", id->str); exit(1); }
            expect(TK_ASSIGN);
            Expr* rhs = parse_expr();
            expect(TK_SEMI);

            // Evaluate RHS into eax (value to store)
            gen_expr(rhs);
            // save value on stack while we load pointer
            fprintf(out, "    push eax\n");
            // load pointer value from var (pointer stored in [varN])
            fprintf(out, "    mov eax, [var%d]\n", ptrslot); // eax = pointer address
            // restore value into ebx and store [eax] = ebx
            fprintf(out, "    pop ebx\n");
            fprintf(out, "    mov [eax], ebx\n");
            continue;
        }

        // simple assignment to variable (ident = expr;) - support for convenience
        if (match(TK_IDENT)) {
            Token* id = peek_tok();
            consume_tok();
            if (match(TK_ASSIGN)) {
                expect(TK_ASSIGN);
                Expr* rhs = parse_expr();
                expect(TK_SEMI);
                int slot = find_var(id->str);
                if (slot == -1) { fprintf(stderr, "unknown variable '%s' in assignment\n", id->str); exit(1); }
                gen_expr(rhs); // result in eax
                fprintf(out, "    mov [var%d], eax ; %s = ...\n", slot, id->str);
                continue;
            }
            else {
                fprintf(stderr, "unsupported statement starting with identifier\n");
                exit(1);
            }
        }

        // return expr;
        if (match(TK_RETURN)) {
            expect(TK_RETURN);
            Expr* e = parse_expr();
            expect(TK_SEMI);
            gen_expr(e);
            fprintf(out, "    ret\n");
            return;
        }

        fprintf(stderr, "unknown statement in function (token %d)\n", tokens[tok_pos].kind);
        exit(1);
    }
}

// ---------- MAIN ----------

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s input.c output.asm\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("open input"); return 1; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    src = malloc(sz + 1);
    if (!src) { perror("malloc"); return 1; }
    fread(src, 1, sz, f);
    src[sz] = 0;
    fclose(f);

    // tokenize once
    tokenize();

    // quick scan: count 'int' declarations (subtract 1 for function header)
    int decl_count = 0;
    for (int i = 0; i < tok_count; ++i) {
        if (tokens[i].kind == TK_INT) decl_count++;
    }
    if (decl_count > 0) decl_count = decl_count - 1;
    if (decl_count < 0) decl_count = 0;

    // prepare output and .bss for exactly the declared variables
    out = fopen(argv[2], "w");
    if (!out) { perror("open output"); return 1; }

    fprintf(out, "[BITS 32]\n");
    fprintf(out, "section .bss\n");
    for (int i = 0; i < decl_count; ++i) {
        fprintf(out, "var%d resd 1\n", i);
    }
    fprintf(out, "section .text\n");
    fprintf(out, "global kernel_main\n");
    fprintf(out, "kernel_main:\n");

    // reset state then parse & emit for real
    tok_pos = 0;
    var_count = 0;
    parse_function_and_emit();

    fclose(out);
    printf("wrote %s\n", argv[2]);
    return 0;
}
