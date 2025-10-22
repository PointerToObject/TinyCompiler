// ----- compiler.c (FULLY FIXED - %d PRINT WORKS!) -----

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

// ---------- GLOBAL ----------
FILE* out; // output file
char* src;
int srcpos = 0;
static int has_return = 0;  // *** TRACK RETURN ***

// ---------- TOKENIZER ----------
typedef enum {
    TK_EOF, TK_INT, TK_INT8, TK_INT16, TK_CHAR, TK_VOID, TK_RETURN, TK_IDENT,
    TK_NUMBER, TK_ASSIGN, TK_PLUS, TK_MINUS, TK_STAR, TK_DIV, TK_AMP,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE, TK_SEMI, TK_PRINT, TK_IF, TK_WHILE,
    TK_EQ, TK_NEQ, TK_LT, TK_GT, TK_LE, TK_GE, TK_STRING, TK_CHAR_LITERAL,
    TK_INC, TK_DEC, TK_COMMA, TK_BREAK
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
    long val;

    // Check for hex prefix (0x or 0X)
    if (src[srcpos] == '0' && (src[srcpos + 1] == 'x' || src[srcpos + 1] == 'X')) {
        srcpos += 2; // Skip "0x"
        start = srcpos;
        while (isxdigit((unsigned char)src[srcpos])) srcpos++;

        if (srcpos == start) {
            fprintf(stderr, "invalid hex literal\n");
            exit(1);
        }

        char tmp[64];
        int len = srcpos - start;
        memcpy(tmp, src + start, len);
        tmp[len] = 0;
        val = strtol(tmp, NULL, 16); // Parse as hex
        printf("[DEBUG] Lexed HEX NUMBER: 0x%lx (%ld)\n", val, val);
    }
    else {
        // Decimal number
        while (isdigit((unsigned char)src[srcpos])) srcpos++;
        char tmp[64];
        int len = srcpos - start;
        memcpy(tmp, src + start, len);
        tmp[len] = 0;
        val = strtol(tmp, NULL, 10);
        printf("[DEBUG] Lexed DECIMAL NUMBER: %ld\n", val);
    }

    add_token((Token) { TK_NUMBER, NULL, val });
}

void lex_char() {
    srcpos++; // Skip opening '
    char value = src[srcpos]; // Get the char
    srcpos++; // Skip the char
    if (src[srcpos] != '\'') { fprintf(stderr, "unterminated char literal\n"); exit(1); }
    srcpos++; // Skip closing '
    add_token((Token) { TK_CHAR_LITERAL, NULL, (long)value });
    printf("[DEBUG] Lexed CHAR: '%c' (val=%ld)\n", value, (long)value);
}

void lex_ident_or_kw() {
    int start = srcpos;
    while (isalnum((unsigned char)src[srcpos]) || src[srcpos] == '_') srcpos++;
    int len = srcpos - start;
    char* id = malloc(len + 1);
    memcpy(id, src + start, len); id[len] = 0;
    Token t = { 0 };
    if (!strcmp(id, "void")) t.kind = TK_VOID;
    else if (!strcmp(id, "int")) t.kind = TK_INT;
    else if (!strcmp(id, "__int8")) t.kind = TK_INT8;
    else if (!strcmp(id, "__int16")) t.kind = TK_INT16;
    else if (!strcmp(id, "char")) t.kind = TK_CHAR;
    else if (!strcmp(id, "return")) t.kind = TK_RETURN;
    else if (!strcmp(id, "print")) t.kind = TK_PRINT;
    else if (!strcmp(id, "if")) t.kind = TK_IF;
    else if (!strcmp(id, "while")) t.kind = TK_WHILE;
    else if (!strcmp(id, "break")) t.kind = TK_BREAK;
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

void tokenize() {
    printf("[*] Tokenizing...\n");
    while (src[srcpos]) {
        char c = src[srcpos];
        if (isspace((unsigned char)c)) { srcpos++; continue; }
        if (c == '/' && src[srcpos + 1] == '/') { srcpos += 2; while (src[srcpos] && src[srcpos] != '\n') srcpos++; continue; }
        if (c == '"') { lex_string(); continue; }
        if (c == '\'') { lex_char(); continue; }
        if (isalpha((unsigned char)c) || c == '_') { lex_ident_or_kw(); continue; }
        if (isdigit((unsigned char)c)) { lex_number(); continue; }

        Token t = { 0 };
        if (c == '=' && src[srcpos + 1] == '=') { t.kind = TK_EQ; srcpos += 2; add_token(t); printf("[DEBUG] Lexed EQ\n"); continue; }
        if (c == '!' && src[srcpos + 1] == '=') { t.kind = TK_NEQ; srcpos += 2; add_token(t); printf("[DEBUG] Lexed NEQ\n"); continue; }
        if (c == '<' && src[srcpos + 1] == '=') { t.kind = TK_LE; srcpos += 2; add_token(t); printf("[DEBUG] Lexed LE\n"); continue; }
        if (c == '>' && src[srcpos + 1] == '=') { t.kind = TK_GE; srcpos += 2; add_token(t); printf("[DEBUG] Lexed GE\n"); continue; }
        if (c == '<') { t.kind = TK_LT; srcpos++; add_token(t); printf("[DEBUG] Lexed LT\n"); continue; }
        if (c == '>') { t.kind = TK_GT; srcpos++; add_token(t); printf("[DEBUG] Lexed GT\n"); continue; }
        if (c == '=') { t.kind = TK_ASSIGN; srcpos++; add_token(t); printf("[DEBUG] Lexed ASSIGN\n"); continue; }
        if (c == '+') {
            if (src[srcpos + 1] == '+') { t.kind = TK_INC; srcpos += 2; add_token(t); printf("[DEBUG] Lexed INC\n"); continue; }
            t.kind = TK_PLUS; srcpos++; add_token(t); printf("[DEBUG] Lexed PLUS\n"); continue;
        }
        if (c == '-') {
            if (src[srcpos + 1] == '-') { t.kind = TK_DEC; srcpos += 2; add_token(t); printf("[DEBUG] Lexed DEC\n"); continue; }
            t.kind = TK_MINUS; srcpos++; add_token(t); printf("[DEBUG] Lexed MINUS\n"); continue;
        }
        if (c == '*') { t.kind = TK_STAR; srcpos++; add_token(t); printf("[DEBUG] Lexed STAR\n"); continue; }
        if (c == '/') { t.kind = TK_DIV; srcpos++; add_token(t); printf("[DEBUG] Lexed DIV\n"); continue; }
        if (c == '&') { t.kind = TK_AMP; srcpos++; add_token(t); printf("[DEBUG] Lexed AMP\n"); continue; }
        if (c == '(') { t.kind = TK_LPAREN; srcpos++; add_token(t); printf("[DEBUG] Lexed LPAREN\n"); continue; }
        if (c == ')') { t.kind = TK_RPAREN; srcpos++; add_token(t); printf("[DEBUG] Lexed RPAREN\n"); continue; }
        if (c == '{') { t.kind = TK_LBRACE; srcpos++; add_token(t); printf("[DEBUG] Lexed LBRACE\n"); continue; }
        if (c == '}') { t.kind = TK_RBRACE; srcpos++; add_token(t); printf("[DEBUG] Lexed RBRACE\n"); continue; }
        if (c == ';') { t.kind = TK_SEMI; srcpos++; add_token(t); printf("[DEBUG] Lexed SEMI\n"); continue; }
        if (c == ',') { t.kind = TK_COMMA; srcpos++; add_token(t); printf("[DEBUG] Lexed COMMA\n"); continue; }
        fprintf(stderr, "unexpected char '%c'\n", c); exit(1);
    }
    add_token((Token) { TK_EOF, NULL, 0 });
    printf("[*] Tokens generated: %d\n", tok_count);
}

// ---------- SYMBOLS ----------
typedef struct {
    char* name;
    int slot;
    int is_ptr;
    int size;
    int points_to_slot;
} Var;
Var vars[256]; int var_count = 0;

int add_var(const char* name, int is_ptr, int size) {
    int idx = var_count++;
    char buf[128];
    snprintf(buf, sizeof(buf), "v_%s", name);
    vars[idx].name = my_strdup(buf);
    vars[idx].slot = idx;
    vars[idx].is_ptr = is_ptr;
    vars[idx].size = size;
    vars[idx].points_to_slot = -1;
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
    EXPR_CMP_EQ, EXPR_CMP_NEQ, EXPR_CMP_LT, EXPR_CMP_GT, EXPR_CMP_LE, EXPR_CMP_GE,
    EXPR_INC, EXPR_DEC, EXPR_CALL
} ExprKind;

typedef struct Expr {
    ExprKind kind; long val; char* name; struct Expr* left; struct Expr* right; int size;
    struct Expr** args; int nargs;
} Expr;

// ---------- FORWARD DECLARATIONS ----------
void debug_expr(Expr* e, int depth);
Expr* parse_expr(int context_size);
void parse_statement();
void emit_expr(Expr* e);

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

Expr* parse_primary(int context_size) {
    Token* t = peek_tok();
    if (match(TK_AMP)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after &\n"); exit(1); }
        Token* id = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_ADDR;
        e->name = my_strdup(id->str);
        e->size = 32;
        printf("[DEBUG] Parsed ADDR(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    if (match(TK_STAR)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        int slot = find_var(id->str);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", id->str); exit(1); }
        if (!vars[slot].is_ptr) { fprintf(stderr, "%s is not a pointer\n", id->str); exit(1); }
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_DEREF;
        e->name = my_strdup(id->str);
        e->size = (vars[slot].points_to_slot >= 0) ? vars[vars[slot].points_to_slot].size : 32;
        printf("[DEBUG] Parsed DEREF(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    if (match(TK_NUMBER) || match(TK_CHAR_LITERAL)) {
        Token* n = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_NUMBER;
        e->val = n->val;
        e->size = (n->kind == TK_CHAR_LITERAL) ? 8 : context_size;
        printf("[DEBUG] Parsed %s(%ld, size=%d)\n",
            n->kind == TK_CHAR_LITERAL ? "CHAR" : "NUMBER",
            e->val, e->size);
        return e;
    }
    if (match(TK_IDENT)) {
        Token* id = consume_tok();
        if (match(TK_LPAREN)) {
            consume_tok(); // consume '('
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = EXPR_CALL;
            e->name = my_strdup(id->str);
            e->nargs = 0;
            e->args = NULL;
            if (!match(TK_RPAREN)) {
                do {
                    Expr* arg = parse_expr(32);
                    e->args = realloc(e->args, sizeof(Expr*) * (e->nargs + 1));
                    e->args[e->nargs++] = arg;
                } while (match(TK_COMMA) && (consume_tok(), 1));
            }
            expect(TK_RPAREN);
            e->size = 32;
            printf("[DEBUG] Parsed CALL %s with %d args\n", e->name, e->nargs);
            return e;
        }
        // *** FIXED: SIMPLE VARIABLE CASE ***
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_VAR;
        e->name = my_strdup(id->str);
        int slot = find_var(id->str);
        e->size = (slot >= 0) ? vars[slot].size : 32;
        printf("[DEBUG] Parsed VAR(%s, size=%d)\n", e->name, e->size);
        return e;
    }
    if (match(TK_INC) || match(TK_DEC)) {
        TokenKind op = consume_tok()->kind;
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after ++/--\n"); exit(1); }
        Token* id = consume_tok();
        int slot = find_var(id->str);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", id->str); exit(1); }
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = (op == TK_INC) ? EXPR_INC : EXPR_DEC;
        e->name = my_strdup(id->str);
        e->size = vars[slot].size;
        printf("[DEBUG] Parsed %s(%s, size=%d)\n", (op == TK_INC) ? "INC" : "DEC", e->name, e->size);
        return e;
    }
    fprintf(stderr, "unexpected token in primary: kind=%d\n", t->kind);
    exit(1);
}

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
    case EXPR_INC: printf("INC(%s, size=%d)\n", e->name, e->size); break;
    case EXPR_DEC: printf("DEC(%s, size=%d)\n", e->name, e->size); break;
    case EXPR_CALL:
        printf("CALL %s (nargs=%d)\n", e->name, e->nargs);
        for (int i = 0; i < e->nargs; i++) debug_expr(e->args[i], depth + 1);
        break;
    case EXPR_CMP_EQ: printf("CMP_EQ(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_NEQ: printf("CMP_NEQ(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LT: printf("CMP_LT(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GT: printf("CMP_GT(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LE: printf("CMP_LE(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GE: printf("CMP_GE(size=%d)\n", e->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    }
}

Expr* parse_factor(int context_size) {
    Expr* e = parse_primary(context_size);
    while (match(TK_STAR) || match(TK_DIV)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_primary(context_size);
        Expr* n = calloc(1, sizeof(Expr));
        n->kind = (op == TK_STAR) ? EXPR_MUL : EXPR_DIV;
        n->left = e;
        n->right = r;
        n->size = (e->size > r->size) ? e->size : r->size;
        e = n;
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_STAR) ? "MUL" : "DIV", n->size);
    }
    return e;
}

Expr* parse_term(int context_size) {
    Expr* e = parse_factor(context_size);
    while (match(TK_PLUS) || match(TK_MINUS)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_factor(context_size);
        Expr* n = calloc(1, sizeof(Expr));
        n->kind = (op == TK_PLUS) ? EXPR_ADD : EXPR_SUB;
        n->left = e;
        n->right = r;
        n->size = (e->size > r->size) ? e->size : r->size;
        e = n;
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_PLUS) ? "ADD" : "SUB", n->size);
    }
    return e;
}

Expr* parse_comparison(int context_size) {
    Expr* e = parse_term(context_size);
    if (match(TK_EQ) || match(TK_NEQ) || match(TK_LT) || match(TK_GT) || match(TK_LE) || match(TK_GE)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_term(context_size);
        Expr* n = calloc(1, sizeof(Expr));
        n->left = e;
        n->right = r;
        n->size = 32;
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

Expr* parse_expr(int context_size) {
    Expr* e = parse_comparison(context_size);
    printf("[DEBUG] Expression tree:\n");
    debug_expr(e, 0);
    return e;
}

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
            fprintf(out, "    mov eax,[%s]\n", varname);
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
    case EXPR_INC: case EXPR_DEC: {
        int slot = find_var(e->name);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", e->name); exit(1); }
        const char* varname = vars[slot].name;
        if (vars[slot].size == 8) {
            fprintf(out, "    mov al,[%s]\n", varname);
            if (e->kind == EXPR_INC) fprintf(out, "    inc al\n");
            else fprintf(out, "    dec al\n");
            fprintf(out, "    mov [%s],al\n", varname);
            fprintf(out, "    movzx eax,al\n");
        }
        else if (vars[slot].size == 16) {
            fprintf(out, "    mov ax,[%s]\n", varname);
            if (e->kind == EXPR_INC) fprintf(out, "    inc ax\n");
            else fprintf(out, "    dec ax\n");
            fprintf(out, "    mov [%s],ax\n", varname);
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    mov eax,[%s]\n", varname);
            if (e->kind == EXPR_INC) fprintf(out, "    inc eax\n");
            else fprintf(out, "    dec eax\n");
            fprintf(out, "    mov [%s],eax\n", varname);
        }
        break;
    }
    case EXPR_CALL: {
        for (int i = e->nargs - 1; i >= 0; --i) {
            emit_expr(e->args[i]);
            fprintf(out, "    push eax\n");
        }
        fprintf(out, "    call %s\n", e->name);
        if (e->nargs > 0) {
            fprintf(out, "    add esp,%d\n", e->nargs * 4);
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
            fprintf(out, "    cmp al,bl\n");
        }
        else if (e->size == 16) {
            fprintf(out, "    cmp ax,bx\n");
        }
        else {
            fprintf(out, "    cmp eax,ebx\n");
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

// ---------- WHILE LOOPS WITH BREAK ----------
void parse_while() {
    int start_lbl = label_id++;
    int end_lbl = label_id++;

    fprintf(out, "while_start%d:\n", start_lbl);
    consume_tok(); expect(TK_LPAREN);
    Expr* cond = parse_expr(32); expect(TK_RPAREN);

    emit_expr(cond);
    fprintf(out, "    je while_end%d\n", end_lbl);

    if (match(TK_LBRACE)) {
        consume_tok();
        while (!match(TK_RBRACE)) {
            if (match(TK_BREAK)) {
                consume_tok(); expect(TK_SEMI);
                fprintf(out, "    jmp while_end%d\n", end_lbl);
                continue;
            }
            parse_statement();
        }
        expect(TK_RBRACE);
    }
    else {
        parse_statement();
    }

    fprintf(out, "    jmp while_start%d\n", start_lbl);
    fprintf(out, "while_end%d:\n", end_lbl);
}

// ---------- IF STATEMENTS ----------
void parse_if() {
    consume_tok();
    expect(TK_LPAREN);
    Expr* cond = parse_expr(32);
    expect(TK_RPAREN);
    int lbl = label_id++;
    emit_expr(cond);
    fprintf(out, "    je skip_label%d\n", lbl);
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

// ---------- STATEMENTS ----------
void parse_statement() {
    Token* t = peek_tok();
    if (match(TK_INT) || match(TK_INT8) || match(TK_INT16) || match(TK_CHAR)) {
        TokenKind type = consume_tok()->kind;
        int size = (type == TK_INT8) ? 8 : (type == TK_CHAR) ? 8 : (type == TK_INT16) ? 16 : 32;
        int is_ptr = 0;
        if (match(TK_STAR)) { consume_tok(); is_ptr = 1; }
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier\n"); exit(1); }
        Token* id = consume_tok();
        int slot = add_var(id->str, is_ptr, size);
        if (match(TK_ASSIGN)) {
            consume_tok();
            Expr* e = parse_expr(size);
            if (is_ptr && e->kind == EXPR_ADDR) {
                int target_slot = find_var(e->name);
                if (target_slot >= 0) {
                    vars[slot].points_to_slot = target_slot;
                    printf("[DEBUG] %s now points to %s\n", id->str, e->name);
                }
            }
            fprintf(out, "    ; assign to %s\n", vars[slot].name);
            emit_expr(e);
            if (vars[slot].size == 8) fprintf(out, "    mov [%s],al\n", vars[slot].name);
            else if (vars[slot].size == 16) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
            else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
        }
        expect(TK_SEMI);
        return;
    }
    if (match(TK_RETURN)) {
        consume_tok();
        Expr* e = parse_expr(32);
        fprintf(out, "    ; return\n");
        emit_expr(e);
        fprintf(out, "    ret\n");
        has_return = 1;  // *** TRACK RETURN ***
        expect(TK_SEMI);
        return;
    }
    if (match(TK_STAR)) { // *ptr = expr
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        int slot = find_var(id->str);
        if (slot < 0 || !vars[slot].is_ptr) { fprintf(stderr, "%s is not a pointer\n", id->str); exit(1); }
        if (!match(TK_ASSIGN)) { fprintf(stderr, "expected = after *%s\n", id->str); exit(1); }
        consume_tok();
        int target_size = (vars[slot].points_to_slot >= 0) ? vars[vars[slot].points_to_slot].size : 32;
        Expr* e = parse_expr(target_size);
        fprintf(out, "    ; assign to *%s\n", vars[slot].name);
        emit_expr(e);
        fprintf(out, "    push eax\n");
        fprintf(out, "    mov eax,[%s]\n", vars[slot].name);
        fprintf(out, "    pop ebx\n");

        if (target_size == 8) fprintf(out, "    mov [eax],bl\n");
        else if (target_size == 16) fprintf(out, "    mov [eax],bx\n");
        else fprintf(out, "    mov [eax],ebx\n");

        expect(TK_SEMI);
        return;
    }
    if (match(TK_IDENT)) {  // i = i + 1 OR i++ OR function call statement
        Token* idtok = consume_tok();
        int slot = find_var(idtok->str);
        if (slot >= 0 && match(TK_ASSIGN)) {  // VARIABLE ASSIGNMENT
            consume_tok();
            Expr* e = parse_expr(vars[slot].size);
            fprintf(out, "    ; assign to %s\n", vars[slot].name);
            emit_expr(e);
            if (vars[slot].size == 8) fprintf(out, "    mov [%s],al\n", vars[slot].name);
            else if (vars[slot].size == 16) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
            else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
            expect(TK_SEMI);
            return;
        }
        // ++/-- as statement?
        if (slot >= 0 && (match(TK_INC) || match(TK_DEC))) {
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = consume_tok()->kind;
            e->name = my_strdup(idtok->str);
            e->size = vars[slot].size;
            fprintf(out, "    ; %s %s\n", (e->kind == EXPR_INC) ? "inc" : "dec", vars[slot].name);
            emit_expr(e);
            expect(TK_SEMI);
            return;
        }
        // function call as a statement? if next token is '(', treat as call
        if (match(TK_LPAREN)) {
            // build a call expr and emit immediately
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = EXPR_CALL;
            e->name = my_strdup(idtok->str);
            e->nargs = 0;
            e->args = NULL;
            consume_tok(); // '('
            if (!match(TK_RPAREN)) {
                while (1) {
                    Expr* arg = parse_expr(32);
                    e->args = realloc(e->args, sizeof(Expr*) * (e->nargs + 1));
                    e->args[e->nargs++] = arg;
                    if (match(TK_COMMA)) {
                        consume_tok();
                        continue;
                    }
                    break;
                }
            }
            expect(TK_RPAREN);
            fprintf(out, "    ; call %s\n", e->name);
            emit_expr(e);
            expect(TK_SEMI);
            return;
        }
        fprintf(stderr, "function calls not yet implemented (or unexpected pattern)\n");
        exit(1);
    }
    if (match(TK_IF)) { parse_if(); return; }
    if (match(TK_WHILE)) { parse_while(); return; }
    // Replace the TK_PRINT case in parse_statement() with this:

// Replace the TK_PRINT case in parse_statement() with this:

// Replace the TK_PRINT case in parse_statement() with this:

    if (match(TK_PRINT)) {
        consume_tok();
        expect(TK_LPAREN);

        // Parse format string
        if (!match(TK_STRING)) { fprintf(stderr, "print expects string literal\n"); exit(1); }
        Token* fmt_tok = consume_tok();
        const char* fmt = fmt_tok->str;
        const char* label = add_string(fmt);

        // *** FIXED: Properly parse comma-separated arguments ***
        int arg_count = 0;
        Expr* args[8];

        // Check if there's a comma (meaning there are arguments)
        if (match(TK_COMMA)) {
            consume_tok();  // Consume the comma after format string
            args[arg_count++] = parse_expr(32);

            // Parse additional arguments
            while (match(TK_COMMA)) {
                consume_tok();
                args[arg_count++] = parse_expr(32);
            }
        }

        expect(TK_RPAREN);
        expect(TK_SEMI);

        // *** EMIT CODE ***
        if (arg_count == 0) {
            fprintf(out, "    lea esi,[%s]\n", label);
            fprintf(out, "    mov edi,0xB8000\n");
            fprintf(out, "    call print_string\n");
        }
        else {
            // Push arguments in LEFT TO RIGHT order (so they appear right on stack)
            for (int i = 0; i < arg_count; i++) {
                emit_expr(args[i]);
                fprintf(out, "    push eax\n");
            }

            // Push format string pointer LAST (so it's at [ebp+8])
            fprintf(out, "    lea eax,[%s]\n", label);
            fprintf(out, "    push eax\n");

            fprintf(out, "    call print_fmt\n");
            fprintf(out, "    add esp,%d\n", (arg_count + 1) * 4);  // Clean up format + args
        }
        return;
    }
    fprintf(stderr, "unexpected statement token: kind=%d\n", t->kind);
    consume_tok();
}

// ---------- FIXED: FULL FUNCTION SUPPORT ----------
void parse_function_and_emit() {
    has_return = 0;  // *** RESET for each function ***

    TokenKind ftype = consume_tok()->kind;
    if (ftype != TK_INT && ftype != TK_VOID) {
        fprintf(stderr, "expected return type (int/void)\n"); exit(1);
    }

    if (!match(TK_IDENT)) {
        fprintf(stderr, "expected function name\n"); exit(1);
    }
    Token* func_name = consume_tok();
    const char* name = my_strdup(func_name->str);
    printf("[DEBUG] Parsing function: %s (returns %s)\n", name, ftype == TK_INT ? "int" : "void");

    expect(TK_LPAREN);

    char* param_names[32];
    int param_types[32];
    int param_count = 0;

    if (!match(TK_RPAREN)) {
        do {
            TokenKind ptype = consume_tok()->kind;
            // *** FIXED: CORRECT LOGIC ***
            if (ptype != TK_INT && ptype != TK_INT8 && ptype != TK_INT16 && ptype != TK_CHAR) {
                fprintf(stderr, "expected parameter type\n"); exit(1);
            }
            param_types[param_count] = (ptype == TK_INT8 || ptype == TK_CHAR) ? 8 :
                (ptype == TK_INT16) ? 16 : 32;

            if (!match(TK_IDENT)) {
                fprintf(stderr, "expected param name\n"); exit(1);
            }
            Token* pid = consume_tok();
            param_names[param_count] = my_strdup(pid->str);

            add_var(param_names[param_count], 0, param_types[param_count]);
            param_count++;
        } while (match(TK_COMMA) && (consume_tok(), 1));
    }

    expect(TK_RPAREN);
    expect(TK_LBRACE);

    fprintf(out, "%s:\n", name);
    if (!strcmp(name, "kernel_main")) {
        fprintf(out, "    push ebp\n    mov ebp, esp\n");
    }

    for (int i = 0; i < param_count; i++) {
        int slot = find_var(param_names[i]);
        fprintf(out, "    mov eax,[esp+%d]\n", 4 + i * 4);
        if (vars[slot].size == 8) fprintf(out, "    mov [%s],al\n", vars[slot].name);
        else if (vars[slot].size == 16) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
        else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
    }

    while (!match(TK_RBRACE)) {
        parse_statement();
    }
    expect(TK_RBRACE);

    if (!strcmp(name, "kernel_main")) {
        fprintf(out, "    mov esp, ebp\n    pop ebp\n");
    }

    // *** FIXED: ONLY emit ret if NO return statement ***
    if (!has_return) {
        fprintf(out, "    ret\n\n");
    }
}

// ---------- SECTIONS ----------
void emit_data_section() {
    if (str_count == 0) return;
    fprintf(out, "\nsection .data\n");
    for (int i = 0; i < str_count; i++) {
        fprintf(out, "%s db ", strings[i].label);
        for (int j = 0; j < strlen(strings[i].str); j++) fprintf(out, "%d,", (unsigned char)strings[i].str[j]);
        fprintf(out, "0\n");
    }
}

void emit_bss_section() {
    fprintf(out, "\nsection .bss\n");
    for (int i = 0; i < var_count; i++) {
        if (vars[i].size == 8) fprintf(out, "%s resb 1\n", vars[i].name);
        else if (vars[i].size == 16) fprintf(out, "%s resw 1\n", vars[i].name);
        else fprintf(out, "%s resd 1\n", vars[i].name);
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
        "    ret\n\n"

        "\nprint_fmt:\n"
        "    push ebp\n"
        "    mov ebp, esp\n"
        "    push ebx\n"
        "    push esi\n"
        "    push edi\n"
        "    mov esi, [ebp+8]      ; format string\n"
        "    mov edi, 0xB8000 + 160 ; next line\n"
        "    lea ebx, [ebp+12]     ; pointer to first argument\n"
        ".fmt_loop:\n"
        "    lodsb\n"
        "    test al, al\n"
        "    jz .fmt_done\n"
        "    cmp al, '%%'\n"
        "    jne .fmt_print_char\n"
        "    lodsb\n"
        "    cmp al, 'd'\n"
        "    jne .fmt_invalid\n"
        "    ; *** PRINT NUMBER - edi is preserved across call ***\n"
        "    push ebx              ; save arg pointer\n"
        "    push esi              ; save string pointer\n"
        "    mov eax, [ebx]        ; get the argument value\n"
        "    call print_number     ; edi will be updated by this\n"
        "    pop esi               ; restore string pointer\n"
        "    pop ebx               ; restore arg pointer\n"
        "    add ebx, 4            ; move to next argument\n"
        "    jmp .fmt_loop\n"
        ".fmt_print_char:\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    jmp .fmt_loop\n"
        ".fmt_invalid:\n"
        "    mov ah, 0x0F\n"
        "    mov al, '?'\n"
        "    stosw\n"
        "    jmp .fmt_loop\n"
        ".fmt_done:\n"
        "    pop edi\n"
        "    pop esi\n"
        "    pop ebx\n"
        "    pop ebp\n"
        "    ret\n\n"

        "\nprint_number:\n"
        "    ; Input: eax = number to print, edi = VGA pointer\n"
        "    ; Output: edi updated to next position\n"
        "    ; Preserves: ebx, esi\n"
        "    push ebx\n"
        "    push ecx\n"
        "    push edx\n"
        "    test eax, eax\n"
        "    jns .num_positive\n"
        "    push eax\n"
        "    mov al, '-'\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    pop eax\n"
        "    neg eax\n"
        ".num_positive:\n"
        "    mov ebx, 10\n"
        "    xor ecx, ecx\n"
        ".num_loop:\n"
        "    xor edx, edx\n"
        "    div ebx\n"
        "    push edx\n"
        "    inc ecx\n"
        "    test eax, eax\n"
        "    jnz .num_loop\n"
        "    test ecx, ecx\n"
        "    jz .num_zero\n"
        ".num_print_loop:\n"
        "    pop eax\n"
        "    add al, '0'\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    loop .num_print_loop\n"
        "    jmp .num_done\n"
        ".num_zero:\n"
        "    mov al, '0'\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        ".num_done:\n"
        "    pop edx\n"
        "    pop ecx\n"
        "    pop ebx\n"
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

    fprintf(out, "[BITS 32]\n\n[org 0x1000]\n\nsection .text\nglobal kernel_main\n\n");
    fprintf(out, "    jmp kernel_main\n\n");

    printf("[*] Parsing ALL functions...\n");
    while (!match(TK_EOF)) {
        parse_function_and_emit();
    }

    emit_print_routine();
    emit_data_section();
    emit_bss_section();

    printf("[*] Done. Assembly written to %s\n", argv[2]);

    fclose(out);
    free(src);
    return 0;
}