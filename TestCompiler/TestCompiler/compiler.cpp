// tinycc_fixed.c
// Minimal C -> x86 32-bit NASM assembly compiler (MSVC-ready)
// Removed print/BIOS code; outputs flat 32-bit asm and calls kernel_main.

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    TK_EOF,
    TK_IDENT,
    TK_NUMBER,
    TK_STRING,
    TK_KW_INT,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE, TK_SEMI, TK_COMMA,
    TK_RETURN
} TokenKind;

typedef struct {
    TokenKind kind;
    char* str; // for ident/string
    long val;  // for number
} Token;

#define MAXTOK 8192
Token tokens[MAXTOK];
int tok_pos = 0;
int tok_count = 0;

char* src;
int srcpos = 0;

void add_token(Token t) {
    if (tok_count >= MAXTOK) { fprintf(stderr, "too many tokens\n"); exit(1); }
    tokens[tok_count++] = t;
}

int startswith(const char* s) {
    return strncmp(src + srcpos, s, strlen(s)) == 0;
}

void lex_string() {
    srcpos++; // skip "
    int start = srcpos;
    while (src[srcpos] && src[srcpos] != '"') {
        if (src[srcpos] == '\\' && src[srcpos + 1]) srcpos += 2; else srcpos++;
    }
    if (src[srcpos] != '"') { fprintf(stderr, "unterminated string\n"); exit(1); }
    int len = srcpos - start;
    char* buf = (char*)malloc(len + 1);
    int di = 0;
    for (int i = 0; i < len; ++i) {
        char c = src[start + i];
        if (c == '\\' && i + 1 < len) {
            char n = src[start + i + 1];
            if (n == 'n') { buf[di++] = '\n'; i++; continue; }
            if (n == 't') { buf[di++] = '\t'; i++; continue; }
            buf[di++] = n; i++; continue;
        }
        buf[di++] = c;
    }
    buf[di] = 0;
    srcpos++; // skip closing "
    Token t; t.kind = TK_STRING; t.str = buf; t.val = 0;
    add_token(t);
}

void lex_ident_or_kw() {
    int start = srcpos;
    while (isalnum((unsigned char)src[srcpos]) || src[srcpos] == '_') srcpos++;
    int len = srcpos - start;
    char* s = (char*)malloc(len + 1);
    memcpy(s, src + start, len); s[len] = 0;
    Token t; t.str = s;
    if (strcmp(s, "int") == 0) t.kind = TK_KW_INT;
    else if (strcmp(s, "return") == 0) t.kind = TK_RETURN;
    else t.kind = TK_IDENT;
    add_token(t);
}

void lex_number() {
    int start = srcpos;
    while (isdigit((unsigned char)src[srcpos])) srcpos++;
    int len = srcpos - start;
    char tmp[64];
    if (len >= (int)sizeof(tmp)) { fprintf(stderr, "number too long\n"); exit(1); }
    memcpy(tmp, src + start, len);
    tmp[len] = 0;
    Token t; t.kind = TK_NUMBER; t.val = strtol(tmp, NULL, 10); t.str = NULL;
    add_token(t);
}

void tokenize() {
    while (src[srcpos]) {
        char c = src[srcpos];
        if (isspace((unsigned char)c)) { srcpos++; continue; }
        if (c == '/' && src[srcpos + 1] == '/') { // line comment
            srcpos += 2;
            while (src[srcpos] && src[srcpos] != '\n') srcpos++;
            continue;
        }
        if (c == '"') { lex_string(); continue; }
        if (isalpha((unsigned char)c) || c == '_') { lex_ident_or_kw(); continue; }
        if (isdigit((unsigned char)c)) { lex_number(); continue; }
        // single char tokens:
        Token t; t.str = NULL; t.val = 0;
        switch (c) {
        case '(': t.kind = TK_LPAREN; break;
        case ')': t.kind = TK_RPAREN; break;
        case '{': t.kind = TK_LBRACE; break;
        case '}': t.kind = TK_RBRACE; break;
        case ';': t.kind = TK_SEMI; break;
        case ',': t.kind = TK_COMMA; break;
        default:
            fprintf(stderr, "unexpected char '%c'\n", c); exit(1);
        }
        srcpos++;
        add_token(t);
    }
    Token t; t.kind = TK_EOF; t.str = NULL; t.val = 0; add_token(t);
}

Token* peek_tok() { return &tokens[tok_pos]; }
Token* consume_tok() { return &tokens[tok_pos++]; }
int accept(TokenKind k) {
    if (peek_tok()->kind == k) { tok_pos++; return 1; }
    return 0;
}
void expect(TokenKind k) {
    if (!accept(k)) { fprintf(stderr, "unexpected token at pos %d\n", tok_pos); exit(1); }
}

// --- AST minimal structures ---

typedef enum { STMT_EXPR, STMT_RETURN } StmtKind;
typedef enum { EXPR_NUMBER, EXPR_CALL } ExprKind;

typedef struct Expr {
    ExprKind kind;
    long val; // number
    char* str; // ident for call
    struct Expr** args; int nargs;
} Expr;

typedef struct Stmt {
    StmtKind kind;
    Expr* expr; // expression statement or return expr
    struct Stmt* next;
} Stmt;

typedef struct Func {
    char* name;
    Stmt* body; // linked list
} Func;

// Parser: supports only `int <ident>() { ... }` and `return <number>;` statements.
// No `print`, no strings in emitted code. If any other statement/expr is encountered - error.
Func* parse_function() {
    if (peek_tok()->kind != TK_KW_INT) return NULL;
    consume_tok(); // int
    if (peek_tok()->kind != TK_IDENT) { fprintf(stderr, "expected ident after int\n"); exit(1); }
    char* fname = _strdup(peek_tok()->str);
    consume_tok(); // ident
    expect(TK_LPAREN);
    expect(TK_RPAREN);
    expect(TK_LBRACE);
    Func* f = (Func*)malloc(sizeof(Func));
    f->name = fname;
    f->body = NULL;
    Stmt** tail = &f->body;
    while (!accept(TK_RBRACE)) {
        if (peek_tok()->kind == TK_RETURN) {
            consume_tok();
            Expr* e = NULL;
            Token* t = peek_tok();
            if (t->kind == TK_NUMBER) {
                e = (Expr*)malloc(sizeof(Expr));
                e->kind = EXPR_NUMBER;
                e->val = t->val;
                e->str = NULL;
                e->args = NULL;
                e->nargs = 0;
                consume_tok();
            }
            else {
                fprintf(stderr, "unsupported return expression (only numeric literals supported)\n"); exit(1);
            }
            expect(TK_SEMI);
            Stmt* s = (Stmt*)malloc(sizeof(Stmt)); s->kind = STMT_RETURN; s->expr = e; s->next = NULL;
            *tail = s; tail = &s->next;
            continue;
        }
        // No other statement types supported currently
        fprintf(stderr, "unsupported statement starting token kind=%d\n", peek_tok()->kind);
        exit(1);
    }
    return f;
}

// --- Codegen (32-bit) ---

void emit_header(FILE* out) {
    fprintf(out, "[BITS 32]\n");
    fprintf(out, "global kernel_main\n");
    fprintf(out, "section .text\n\n");
}

// generate code for a single expression (only supports number for return)
void gen_expr(FILE* out, Expr* e) {
    if (e->kind == EXPR_NUMBER) {
        fprintf(out, "    mov eax, %ld\n", e->val);
        return;
    }
    fprintf(stderr, "unsupported gen_expr kind=%d\n", e->kind); exit(1);
}

void gen_stmt(FILE* out, Stmt* s) {
    if (!s) return;
    if (s->kind == STMT_EXPR) {
        gen_expr(out, s->expr);
    }
    else if (s->kind == STMT_RETURN) {
        if (s->expr->kind == EXPR_NUMBER) {
            fprintf(out, "    mov eax, %ld\n", s->expr->val);
        }
        else {
            fprintf(stderr, "unsupported return expr\n"); exit(1);
        }
        fprintf(out, "    ret\n");
    }
}

void gen_function(FILE* out, Func* f) {
    // emit kernel_main label regardless of function name (your bootloader expects kernel_main)
    fprintf(out, "kernel_main:\n");
    Stmt* s = f->body;
    while (s) {
        gen_stmt(out, s);
        s = s->next;
    }
    // default return 0 if no explicit return
    //fprintf(out, "    mov eax, 0\n");
    //fprintf(out, "    ret\n\n");
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s input.c out.asm\n", argv[0]);
        return 1;
    }
    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("open"); return 1; }
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    src = (char*)malloc(sz + 1); if (!src) { fprintf(stderr, "malloc failed\n"); return 1; }
    fread(src, 1, sz, f); src[sz] = 0; fclose(f);

    tokenize();
    tok_pos = 0;
    Func* fn = parse_function();
    if (!fn) { fprintf(stderr, "no function parsed\n"); return 1; }

    FILE* out = fopen(argv[2], "wb");
    if (!out) { perror("out"); return 1; }

    emit_header(out);

    // Emit a tiny start that calls kernel_main (optional — bootloader can also jump directly)
    fprintf(out, "start:\n");
    fprintf(out, "    call kernel_main\n");
    fprintf(out, "    jmp $ ; hang\n\n");

    // emit function body
    gen_function(out, fn);

    fclose(out);
    printf("wrote %s\n", argv[2]);
    return 0;
}
