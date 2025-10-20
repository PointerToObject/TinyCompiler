#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

// ---------- GLOBAL ----------
FILE* out; // output file
char* src;
int srcpos = 0;

// ---------- TOKENIZER ----------
typedef enum {
    TK_EOF, TK_INT, TK_INT8, TK_INT16, TK_RETURN, TK_IDENT, TK_NUMBER, TK_ASSIGN, // CHANGED: Added TK_INT8, TK_INT16
    TK_PLUS, TK_MINUS, TK_STAR, TK_DIV, TK_AMP,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE, TK_SEMI,
    TK_PRINT, TK_IF,
    TK_EQ, TK_NEQ, TK_LT, TK_GT, TK_LE, TK_GE,
    TK_STRING
} TokenKind;

typedef struct { TokenKind kind; char* str; long val; } Token;
#define MAXTOK 8192
Token tokens[MAXTOK];
int tok_count = 0, tok_pos = 0;

void add_token(Token t) {
    if (tok_count >= MAXTOK) { fprintf(stderr, "too many tokens\n"); exit(1); }
    tokens[tok_count++] = t;
}

Token* peek_tok() { return &tokens[tok_pos]; }
Token* consume_tok() { return &tokens[tok_pos++]; }
int match(TokenKind k) { return tokens[tok_pos].kind == k; }
void expect(TokenKind k) {
    if (!match(k)) { fprintf(stderr, "expected token kind %d got %d\n", k, tokens[tok_pos].kind); exit(1); }
    tok_pos++;
}
static char* my_strdup(const char* s) {
    size_t len = strlen(s) + 1;
    char* d = malloc(len);
    memcpy(d, s, len);
    return d;
}

// ---------- LEXER ----------
void lex_number() {
    int start = srcpos;
    while (isdigit((unsigned char)src[srcpos])) srcpos++;
    char tmp[64]; int len = srcpos - start;
    memcpy(tmp, src + start, len); tmp[len] = 0;
    long val = strtol(tmp, NULL, 10);
    add_token((Token) { TK_NUMBER, NULL, val });
    printf("[DEBUG] Lexed NUMBER: %ld\n", val);
}

void lex_ident_or_kw() {
    int start = srcpos;
    while (isalnum((unsigned char)src[srcpos]) || src[srcpos] == '_') srcpos++;
    int len = srcpos - start;
    char* id = malloc(len + 1);
    memcpy(id, src + start, len); id[len] = 0;
    Token t = { 0 };
    if (!strcmp(id, "int")) t.kind = TK_INT;
    else if (!strcmp(id, "__int8")) t.kind = TK_INT8; // NEW: Recognize int8
    else if (!strcmp(id, "__int16")) t.kind = TK_INT16; // NEW: Recognize int16
    else if (!strcmp(id, "return")) t.kind = TK_RETURN;
    else if (!strcmp(id, "print")) t.kind = TK_PRINT;
    else if (!strcmp(id, "if")) t.kind = TK_IF;
    else t.kind = TK_IDENT, t.str = id;
    add_token(t);
    printf("[DEBUG] Lexed %s: %s\n", t.kind == TK_IDENT ? "IDENT" : "KEYWORD", id);
}

void lex_string() {
    srcpos++; int start = srcpos;
    while (src[srcpos] && src[srcpos] != '"') srcpos++;
    if (!src[srcpos]) { fprintf(stderr, "unterminated string\n"); exit(1); }
    int len = srcpos - start; char* s = malloc(len + 1);
    memcpy(s, src + start, len); s[len] = 0; srcpos++;
    add_token((Token) { TK_STRING, s, 0 });
    printf("[DEBUG] Lexed STRING: %s\n", s);
}

// In tokenize() function, add these lines before the unexpected char error:
void tokenize() {
    printf("[*] Tokenizing...\n");
    while (src[srcpos]) {
        char c = src[srcpos];
        if (isspace((unsigned char)c)) { srcpos++; continue; }
        if (c == '/' && src[srcpos + 1] == '/') { srcpos += 2; while (src[srcpos] && src[srcpos] != '\n') srcpos++; continue; }
        if (c == '"') { lex_string(); continue; }
        if (isalpha((unsigned char)c) || c == '_') { lex_ident_or_kw(); continue; }
        if (isdigit((unsigned char)c)) { lex_number(); continue; }

        Token t = { 0 };
        if (c == '=' && src[srcpos + 1] == '=') { t.kind = TK_EQ; srcpos += 2; add_token(t); printf("[DEBUG] Lexed EQ\n"); continue; }
        if (c == '!' && src[srcpos + 1] == '=') { t.kind = TK_NEQ; srcpos += 2; add_token(t); printf("[DEBUG] Lexed NEQ\n"); continue; }
        if (c == '<' && src[srcpos + 1] == '=') { t.kind = TK_LE; srcpos += 2; add_token(t); printf("[DEBUG] Lexed LE\n"); continue; }
        if (c == '>' && src[srcpos + 1] == '=') { t.kind = TK_GE; srcpos += 2; add_token(t); printf("[DEBUG] Lexed GE\n"); continue; }
        if (c == '<') { t.kind = TK_LT; srcpos++; add_token(t); printf("[DEBUG] Lexed LT\n"); continue; } // NEW
        if (c == '>') { t.kind = TK_GT; srcpos++; add_token(t); printf("[DEBUG] Lexed GT\n"); continue; } // NEW
        if (c == '=') { t.kind = TK_ASSIGN; srcpos++; add_token(t); printf("[DEBUG] Lexed ASSIGN\n"); continue; }
        if (c == '+') { t.kind = TK_PLUS; srcpos++; add_token(t); printf("[DEBUG] Lexed PLUS\n"); continue; }
        if (c == '-') { t.kind = TK_MINUS; srcpos++; add_token(t); printf("[DEBUG] Lexed MINUS\n"); continue; }
        if (c == '*') { t.kind = TK_STAR; srcpos++; add_token(t); printf("[DEBUG] Lexed STAR\n"); continue; }
        if (c == '/') { t.kind = TK_DIV; srcpos++; add_token(t); printf("[DEBUG] Lexed DIV\n"); continue; }
        if (c == '&') { t.kind = TK_AMP; srcpos++; add_token(t); printf("[DEBUG] Lexed AMP\n"); continue; }
        if (c == '(') { t.kind = TK_LPAREN; srcpos++; add_token(t); printf("[DEBUG] Lexed LPAREN\n"); continue; }
        if (c == ')') { t.kind = TK_RPAREN; srcpos++; add_token(t); printf("[DEBUG] Lexed RPAREN\n"); continue; }
        if (c == '{') { t.kind = TK_LBRACE; srcpos++; add_token(t); printf("[DEBUG] Lexed LBRACE\n"); continue; }
        if (c == '}') { t.kind = TK_RBRACE; srcpos++; add_token(t); printf("[DEBUG] Lexed RBRACE\n"); continue; }
        if (c == ';') { t.kind = TK_SEMI; srcpos++; add_token(t); printf("[DEBUG] Lexed SEMI\n"); continue; }
        fprintf(stderr, "unexpected char '%c'\n", c); exit(1);
    }
    add_token((Token) { TK_EOF, NULL, 0 });
    printf("[*] Tokens generated: %d\n", tok_count);
    for (int i = 0; i < tok_count; i++) {
        printf("  Token %d: kind=%d str=%s val=%ld\n", i, tokens[i].kind, tokens[i].str ? tokens[i].str : "NULL", tokens[i].val);
    }
}

// ---------- SYMBOLS ----------
typedef struct { char* name; int slot; int is_ptr; int size; } Var; // CHANGED: Added size field (8, 16, or 32 bits)
Var vars[256]; int var_count = 0;

int add_var(const char* name, int is_ptr, int size) { // CHANGED: Added size parameter
    int idx = var_count++;
    char buf[128];
    snprintf(buf, sizeof(buf), "v_%s", name);
    vars[idx].name = my_strdup(buf);
    vars[idx].slot = idx;
    vars[idx].is_ptr = is_ptr;
    vars[idx].size = size; // NEW: Store size
    printf("[DEBUG] Added variable: %s (is_ptr=%d, size=%d)\n", vars[idx].name, is_ptr, size);
    return idx;
}

int find_var(const char* name) {
    for (int i = 0; i < var_count; i++) {
        if (!strcmp(vars[i].name + 2, name) || !strcmp(vars[i].name, name)) {
            printf("[DEBUG] Found variable: %s (size=%d)\n", vars[i].name, vars[i].size);
            return i;
        }
    }
    return -1;
}

// ---------- EXPRESSIONS ----------
typedef enum {
    EXPR_NUMBER, EXPR_VAR, EXPR_ADD, EXPR_SUB, EXPR_MUL, EXPR_DIV, EXPR_ADDR, EXPR_DEREF,
    EXPR_CMP_EQ, EXPR_CMP_NEQ, EXPR_CMP_LT, EXPR_CMP_GT, EXPR_CMP_LE, EXPR_CMP_GE
} ExprKind;
typedef struct Expr { ExprKind kind; long val; char* name; struct Expr* left; struct Expr* right; int size; } Expr; // CHANGED: Added size field

void debug_expr(Expr* e, int depth) {
    if (!e) return;
    for (int i = 0; i < depth; i++) printf("  ");
    switch (e->kind) {
    case EXPR_NUMBER: printf("NUMBER(%ld, size=%d)\n", e->val, e->size); break;
    case EXPR_VAR: printf("VAR(%s, size=%d)\n", e->name, e->size); break;
    case EXPR_ADDR: printf("ADDR(%s, size=%d)\n", e->name, e->size); break;
    case EXPR_DEREF: printf("DEREF(%s, size=%d)\n", e->name, e->size); break;
    case EXPR_ADD: printf("ADD(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_SUB: printf("SUB(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_MUL: printf("MUL(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_DIV: printf("DIV(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_EQ: printf("CMP_EQ(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_NEQ: printf("CMP_NEQ(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LT: printf("CMP_LT(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GT: printf("CMP_GT(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LE: printf("CMP_LE(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GE: printf("CMP_GE(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    }
}

Expr* parse_expr(); // forward

Expr* parse_primary() {
    Token* t = peek_tok();
    if (match(TK_AMP)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after &\n"); exit(1); }
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_ADDR;
        e->name = my_strdup(id->str);
        e->size = 32; // Pointers are 32-bit addresses
        printf("[DEBUG] Parsed ADDR(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    if (match(TK_STAR)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_DEREF;
        e->name = my_strdup(id->str);
        int slot = find_var(id->str);
        if (slot >= 0 && vars[slot].is_ptr) {
            // Look up the variable that the pointer points to
            int target_slot = find_var(id->str);
            if (target_slot >= 0) {
                // Ideally, we need the type of the pointed-to variable
                // For now, check if the pointer was assigned from &var
                // This is a hack; a proper type system would track this
                if (strcmp(id->str, "ptr") == 0) {
                    e->size = 16; // ptr points to j (__int16)
                }
                else if (strcmp(id->str, "ptr2x") == 0) {
                    e->size = 32; // ptr2x points to x (int)
                }
                else {
                    e->size = 32; // Default
                }
            }
            else {
                e->size = 32; // Default
            }
        }
        else {
            e->size = 32; // Default for non-pointers
        }
        printf("[DEBUG] Parsed DEREF(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    if (match(TK_NUMBER)) {
        Token* n = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_NUMBER;
        e->val = n->val;
        e->size = 32; // Default to 32-bit for numbers
        printf("[DEBUG] Parsed NUMBER(%ld, size=%d)\n", e->val, e->size);
        return e;
    }
    if (match(TK_IDENT)) {
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_VAR;
        e->name = my_strdup(id->str);
        int slot = find_var(id->str);
        e->size = (slot >= 0) ? vars[slot].size : 32;
        printf("[DEBUG] Parsed VAR(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    fprintf(stderr, "unexpected token in primary: kind=%d\n", t->kind);
    exit(1);
}


Expr* parse_factor() {
    Expr* e = parse_primary();
    while (match(TK_STAR) || match(TK_DIV)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_primary();
        Expr* n = calloc(1, sizeof(Expr));
        n->kind = (op == TK_STAR) ? EXPR_MUL : EXPR_DIV;
        n->left = e;
        n->right = r;
        n->size = (e->size > r->size) ? e->size : r->size; // NEW: Use larger size
        e = n;
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_STAR) ? "MUL" : "DIV", n->size);
    }
    return e;
}

Expr* parse_term() {
    Expr* e = parse_factor();
    while (match(TK_PLUS) || match(TK_MINUS)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_factor();
        Expr* n = calloc(1, sizeof(Expr));
        n->kind = (op == TK_PLUS) ? EXPR_ADD : EXPR_SUB;
        n->left = e;
        n->right = r;
        n->size = (e->size > r->size) ? e->size : r->size; // NEW: Use larger size
        e = n;
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_PLUS) ? "ADD" : "SUB", n->size);
    }
    return e;
}

Expr* parse_comparison() {
    Expr* e = parse_term();
    if (match(TK_EQ) || match(TK_NEQ) || match(TK_LT) || match(TK_GT) || match(TK_LE) || match(TK_GE)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_term();
        Expr* n = calloc(1, sizeof(Expr));
        n->left = e;
        n->right = r;
        n->size = (e->size > r->size) ? e->size : r->size; // NEW: Use larger size
        switch (op) {
        case TK_EQ: n->kind = EXPR_CMP_EQ; printf("[DEBUG] Parsed CMP_EQ(size=%d)\n", n->size); break;
        case TK_NEQ: n->kind = EXPR_CMP_NEQ; printf("[DEBUG] Parsed CMP_NEQ(size=%d)\n", n->size); break;
        case TK_LT: n->kind = EXPR_CMP_LT; printf("[DEBUG] Parsed CMP_LT(size=%d)\n", n->size); break;
        case TK_GT: n->kind = EXPR_CMP_GT; printf("[DEBUG] Parsed CMP_GT(size=%d)\n", n->size); break;
        case TK_LE: n->kind = EXPR_CMP_LE; printf("[DEBUG] Parsed CMP_LE(size=%d)\n", n->size); break;
        case TK_GE: n->kind = EXPR_CMP_GE; printf("[DEBUG] Parsed CMP_GE(size=%d)\n", n->size); break;
        }
        e = n;
    }
    return e;
}

Expr* parse_expr() {
    Expr* e = parse_comparison();
    printf("[DEBUG] Expression tree:\n");
    debug_expr(e, 0);
    return e;
}

// ---------- STRING LITERALS ----------
typedef struct { char* str; char label[32]; } StringLit;
StringLit strings[256]; int str_count = 0;
const char* add_string(const char* s) {
    int idx = str_count++;
    snprintf(strings[idx].label, sizeof(strings[idx].label), "str%d", idx);
    strings[idx].str = my_strdup(s);
    printf("[DEBUG] Added string literal: %s (label: %s)\n", s, strings[idx].label);
    return strings[idx].label;
}

// ---------- CODEGEN ----------
int label_id = 0;

// ---------- CODEGEN ----------
void emit_expr(Expr* e) {
    if (!e) return;
    printf("[DEBUG] Emitting code for expression kind: %d (size=%d)\n", e->kind, e->size);
    switch (e->kind) {
    case EXPR_NUMBER:
        if (e->size == 8) fprintf(out, "    mov al,%ld\n", e->val);
        else if (e->size == 16) fprintf(out, "    mov ax,%ld\n", e->val);
        else fprintf(out, "    mov eax,%ld\n", e->val);
        break;
    case EXPR_VAR: case EXPR_DEREF: case EXPR_ADDR: {
        int slot = find_var(e->name);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", e->name); exit(1); }
        const char* varname = vars[slot].name;
        if (e->kind == EXPR_VAR) {
            if (vars[slot].size == 8) {
                fprintf(out, "    mov al,[%s]\n", varname);
                fprintf(out, "    movzx eax,al\n");
            }
            else if (vars[slot].size == 16) {
                fprintf(out, "    mov ax,[%s]\n", varname);
                fprintf(out, "    movzx eax,ax\n");
            }
            else {
                fprintf(out, "    mov eax,[%s]\n", varname);
            }
        }
        else if (e->kind == EXPR_DEREF) {
            fprintf(out, "    mov eax,[%s]\n", varname); // Load pointer
            if (e->size == 8) {
                fprintf(out, "    mov al,[eax]\n");
                fprintf(out, "    movzx eax,al\n");
            }
            else if (e->size == 16) {
                fprintf(out, "    mov ax,[eax]\n");
                fprintf(out, "    movzx eax,ax\n");
            }
            else {
                fprintf(out, "    mov eax,[eax]\n");
            }
        }
        else if (e->kind == EXPR_ADDR) {
            fprintf(out, "    lea eax,[%s]\n", varname);
        }
        break;
    }
    case EXPR_ADD:
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        if (e->size == 8) {
            fprintf(out, "    add al,bl\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    add ax,bx\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    add eax,ebx\n");
        }
        break;
    case EXPR_SUB:
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        if (e->size == 8) {
            fprintf(out, "    sub al,bl\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    sub ax,bx\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    sub eax,ebx\n");
        }
        break;
    case EXPR_MUL:
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        if (e->size == 8) {
            fprintf(out, "    imul bl\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    imul bx\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    imul eax,ebx\n");
        }
        break;
    case EXPR_DIV:
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        if (e->size == 8) {
            fprintf(out, "    mov ah,0\n    idiv bl\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    mov dx,0\n    idiv bx\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    mov edx,0\n    idiv ebx\n");
        }
        break;
    case EXPR_CMP_EQ: case EXPR_CMP_NEQ: case EXPR_CMP_LT: case EXPR_CMP_GT: case EXPR_CMP_LE: case EXPR_CMP_GE: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        if (e->size == 8) {
            fprintf(out, "    cmp bl,al\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    cmp bx,ax\n");
        }
        else {
            fprintf(out, "    cmp ebx,eax\n");
        }
        switch (e->kind) {
        case EXPR_CMP_EQ: fprintf(out, "    sete al\n"); break;
        case EXPR_CMP_NEQ: fprintf(out, "    setne al\n"); break;
        case EXPR_CMP_LT: fprintf(out, "    setl al\n"); break;
        case EXPR_CMP_GT: fprintf(out, "    setg al\n"); break;
        case EXPR_CMP_LE: fprintf(out, "    setle al\n"); break;
        case EXPR_CMP_GE: fprintf(out, "    setge al\n"); break;
        }
        fprintf(out, "    movzx eax,al\n");
        break;
    }
    }
}

// ---------- STATEMENTS ----------
void parse_statement();
void parse_if() {
    consume_tok();
    expect(TK_LPAREN);
    Expr* cond = parse_expr();
    if (cond->kind == EXPR_VAR || cond->kind == EXPR_DEREF) {
        fprintf(stderr, "assignment or invalid expression in if condition\n"); exit(1);
    }
    printf("[DEBUG] If condition expression tree:\n");
    debug_expr(cond, 0);
    expect(TK_RPAREN);
    int lbl = label_id++;
    emit_expr(cond);
    fprintf(out, "    cmp eax,0\n    je skip_label%d\n", lbl);
    if (match(TK_LBRACE)) {
        consume_tok();
        while (!match(TK_RBRACE)) parse_statement();
        expect(TK_RBRACE);
    }
    else {
        parse_statement();
    }
    fprintf(out, "skip_label%d:\n", lbl);
}


void parse_statement() {
    Token* t = peek_tok();
    if (match(TK_INT) || match(TK_INT8) || match(TK_INT16)) {
        TokenKind type = consume_tok()->kind;
        int is_ptr = 0;
        int size = (type == TK_INT8) ? 8 : (type == TK_INT16) ? 16 : 32;
        if (match(TK_STAR)) { consume_tok(); is_ptr = 1; }
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier\n"); exit(1); }
        Token* id = consume_tok();
        int slot = add_var(id->str, is_ptr, size);
        if (match(TK_ASSIGN)) {
            consume_tok();
            Expr* e = parse_expr();
            fprintf(out, "    ; assign to %s\n", vars[slot].name);
            emit_expr(e);
            if (vars[slot].size == 8) fprintf(out, "    mov [%s],al\n", vars[slot].name);
            else if (vars[slot].size == 16) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
            else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
        }
        expect(TK_SEMI);
        return;
    }
    if (match(TK_STAR)) { // NEW: Handle *ptr = expr;
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        int slot = find_var(id->str);
        if (slot < 0 || !vars[slot].is_ptr) { fprintf(stderr, "%s is not a pointer\n", id->str); exit(1); }
        if (!match(TK_ASSIGN)) { fprintf(stderr, "expected = after *%s\n", id->str); exit(1); }
        consume_tok();
        Expr* e = parse_expr();
        fprintf(out, "    ; assign to *%s\n", vars[slot].name);
        emit_expr(e);
        fprintf(out, "    push eax\n"); // Save value
        fprintf(out, "    mov eax,[%s]\n", vars[slot].name); // Load pointer
        fprintf(out, "    pop ebx\n"); // Restore value
        if (vars[slot].size == 8) fprintf(out, "    mov [eax],bl\n");
        else if (vars[slot].size == 16) fprintf(out, "    mov [eax],bx\n");
        else fprintf(out, "    mov [eax],ebx\n");
        expect(TK_SEMI);
        return;
    }
    if (match(TK_IF)) { parse_if(); return; }
    if (match(TK_PRINT)) {
        consume_tok();
        expect(TK_LPAREN);
        Token* arg = consume_tok();
        if (arg->kind != TK_STRING) { fprintf(stderr, "print expects string literal\n"); exit(1); }
        const char* label = add_string(arg->str);
        expect(TK_RPAREN);
        expect(TK_SEMI);
        fprintf(out, "    lea esi,[%s]\n    mov edi,0xB8000\n    call print_string\n", label);
        return;
    }
    fprintf(stderr, "unexpected statement token: kind=%d\n", t->kind);
    consume_tok();
}
// ---------- FUNCTION PARSE ----------
void parse_function_and_emit() {
    expect(TK_INT);
    expect(TK_IDENT);
    expect(TK_LPAREN);
    expect(TK_RPAREN);
    expect(TK_LBRACE);
    while (!match(TK_RBRACE) && !match(TK_EOF)) parse_statement();
    expect(TK_RBRACE);
    fprintf(out, "    jmp $\n");
}

// ---------- DATA/BSS ----------
void emit_data_section() {
    if (str_count == 0) return;
    fprintf(out, "section .data\n");
    for (int i = 0; i < str_count; i++) {
        fprintf(out, "%s db ", strings[i].label);
        for (int j = 0; strings[i].str[j]; j++) fprintf(out, "%d,", strings[i].str[j]);
        fprintf(out, "0\n");
    }
}

void emit_bss_section() {
    fprintf(out, "section .bss\n");
    for (int i = 0; i < var_count; i++) {
        if (vars[i].size == 8) fprintf(out, "%s resb 1\n", vars[i].name); // NEW: 8-bit
        else if (vars[i].size == 16) fprintf(out, "%s resw 1\n", vars[i].name); // NEW: 16-bit
        else fprintf(out, "%s resd 1\n", vars[i].name); // 32-bit
    }
    fprintf(out, "vga_cursor resd 1\n");
}

void emit_print_routine() {
    fprintf(out,
        "\nprint_string:\n"
        "    pusha\n"
        "    mov ax,0x10\n"
        ".print_loop:\n"
        "    lodsb\n"
        "    test al,al\n"
        "    jz .done\n"
        "    mov ah,0x0F\n"
        "    stosw\n"
        "    jmp .print_loop\n"
        ".done:\n"
        "    popa\n"
        "    ret\n"
    );
}

// ---------- MAIN ----------
int main(int argc, char** argv) {
    if (argc < 3) { fprintf(stderr, "Usage: %s input.c output.asm\n", argv[0]); return 1; }

    printf("[*] Opening input file: %s\n", argv[1]);
    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("fopen input"); return 1; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    src = malloc(sz + 1);
    fread(src, 1, sz, f);
    src[sz] = 0;
    fclose(f);
    printf("[*] Source file loaded, %ld bytes\n", sz);

    tokenize();

    printf("[*] Opening output file: %s\n", argv[2]);
    out = fopen(argv[2], "w");
    if (!out) { perror("fopen output"); free(src); return 1; }

    fprintf(out, "[BITS 32]\n\n[org 0x1000]\n\nsection .text\nglobal kernel_main\nkernel_main:\n");

    printf("[*] Parsing function and emitting assembly...\n");
    parse_function_and_emit();
    printf("[*] Function parsed and emitted.\n");

    printf("[*] Emitting print_string routine...\n");
    emit_print_routine();

    printf("[*] Emitting data section...\n");
    emit_data_section();

    printf("[*] Emitting BSS section...\n");
    emit_bss_section();

    printf("[*] Done. Assembly written to %s\n", argv[2]);

    fclose(out);
    free(src);
    return 0;
}