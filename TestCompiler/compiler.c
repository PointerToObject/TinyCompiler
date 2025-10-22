// ----- compiler.c (STRUCT INIT + INLINE ASM + VGA + BITWISE + HEX + CHAR PRINT) -----

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
    TK_INC, TK_DEC, TK_COMMA, TK_BREAK, TK_STRUCT, TK_DOT, TK_ARROW,
    TK_ASM, TK_COLON,
    // *** NEW: BITWISE OPERATORS ***
    TK_SHL, TK_SHR, TK_OR, TK_AND
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
    else if (!strcmp(id, "struct")) t.kind = TK_STRUCT;
    else if (!strcmp(id, "__asm__")) t.kind = TK_ASM;
    else t.kind = TK_IDENT, t.str = id;
    add_token(t);
    printf("[DEBUG] Lexed %s: %s\n", t.kind == TK_IDENT ? "IDENT" : "KEYWORD", id);
}

void lex_string() {
    srcpos++; // skip "
    int start = srcpos;
    while (src[srcpos] && src[srcpos] != '"') {
        srcpos++;
    }
    int raw_len = srcpos - start;
    char* raw = malloc(raw_len + 1);
    memcpy(raw, src + start, raw_len);
    raw[raw_len] = 0;

    // *** FIXED: PROCESS ESCAPES + SKIP WIDTH DIGITS ***
    char* processed = malloc(raw_len + 1);
    int j = 0;
    for (int i = 0; i < raw_len; i++) {
        if (raw[i] == '\\' && i + 1 < raw_len && raw[i + 1] == 'n') {
            processed[j++] = '\n';  // *** CONVERT \n ***
            i++; // skip 'n'
        }
        // *** NEW: SKIP WIDTH DIGITS AFTER %x ***
        else if (raw[i] == '%' && i + 2 < raw_len && raw[i + 1] == 'x' &&
            isdigit((unsigned char)raw[i + 2])) {
            processed[j++] = '%';   // Keep %
            processed[j++] = 'x';   // Keep x
            i += 2;                 // SKIP DIGITS
        }
        else {
            processed[j++] = raw[i];
        }
    }
    processed[j] = 0;

    char* s = my_strdup(processed);
    srcpos++; // skip closing "

    add_token((Token) { TK_STRING, s, 0 });
    printf("[DEBUG] Lexed STRING: '%s' (raw: '%s')\n", s, raw);
    free(raw);
    free(processed);
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

        // *** NEW: BITWISE OPERATORS ***
        if (c == '<' && src[srcpos + 1] == '<') { t.kind = TK_SHL; srcpos += 2; add_token(t); printf("[DEBUG] Lexed SHL\n"); continue; }
        if (c == '>' && src[srcpos + 1] == '>') { t.kind = TK_SHR; srcpos += 2; add_token(t); printf("[DEBUG] Lexed SHR\n"); continue; }
        if (c == '|') { t.kind = TK_OR; srcpos++; add_token(t); printf("[DEBUG] Lexed OR\n"); continue; }
        if (c == '&' && src[srcpos + 1] != '&') { t.kind = TK_AND; srcpos++; add_token(t); printf("[DEBUG] Lexed AND\n"); continue; }

        if (c == ':' && src[srcpos + 1] == ':') { t.kind = TK_COLON; srcpos += 2; add_token(t); printf("[DEBUG] Lexed COLON\n"); continue; }
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
            if (src[srcpos + 1] == '>') { t.kind = TK_ARROW; srcpos += 2; add_token(t); printf("[DEBUG] Lexed ARROW\n"); continue; }
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
        if (c == '.') { t.kind = TK_DOT; srcpos++; add_token(t); printf("[DEBUG] Lexed DOT\n"); continue; }
        fprintf(stderr, "unexpected char '%c'\n", c); exit(1);
    }
    add_token((Token) { TK_EOF, NULL, 0 });
    printf("[*] Tokens generated: %d\n", tok_count);
}

// ---------- TYPES ----------
typedef enum { TY_VOID, TY_INT, TY_CHAR, TY_PTR, TY_STRUCT } TypeKind;

typedef struct Field {
    char* name;
    struct Type* type;
    int offset;
} Field;

typedef struct Type {
    TypeKind kind;
    int size;
    struct Type* ptr_to;
    char* name;
    Field** fields;
    int nfields;
} Type;

Type types[256];
int type_count = 0;

Type* add_type(TypeKind k, int size, Type* ptr_to, char* name, Field** fields, int nfields) {
    Type* t = &types[type_count++];
    t->kind = k;
    t->size = size;
    t->ptr_to = ptr_to;
    t->name = name ? my_strdup(name) : NULL;
    t->fields = fields;
    t->nfields = nfields;
    return t;
}

Type* find_type(const char* name) {
    for (int i = 0; i < type_count; i++) {
        if (types[i].name && !strcmp(types[i].name, name)) {
            return &types[i];
        }
    }
    return NULL;
}

Type* ty_void;
Type* ty_int;
Type* ty_int8;
Type* ty_int16;
Type* ty_char;

void init_types() {
    ty_void = add_type(TY_VOID, 0, NULL, NULL, NULL, 0);
    ty_int = add_type(TY_INT, 4, NULL, NULL, NULL, 0);
    ty_int8 = add_type(TY_INT, 1, NULL, NULL, NULL, 0);
    ty_int16 = add_type(TY_INT, 2, NULL, NULL, NULL, 0);
    ty_char = add_type(TY_CHAR, 1, NULL, NULL, NULL, 0);
}

// *** FIXED: STRUCT TYPE SUPPORT ***
Type* parse_base_type() {
    Token* t = peek_tok();

    // *** HANDLE STRUCT NAMES ***
    if (t->kind == TK_IDENT) {
        Type* st = find_type(t->str);
        if (st && st->kind == TY_STRUCT) {
            consume_tok();
            printf("[DEBUG] Using struct type: %s (size=%d)\n", t->str, st->size);
            return st;
        }
    }

    // *** PRIMITIVE TYPES ***
    if (match(TK_INT)) { consume_tok(); return ty_int; }
    if (match(TK_INT8)) { consume_tok(); return ty_int8; }
    if (match(TK_INT16)) { consume_tok(); return ty_int16; }
    if (match(TK_CHAR)) { consume_tok(); return ty_char; }
    if (match(TK_VOID)) { consume_tok(); return ty_void; }

    fprintf(stderr, "unexpected type '%s'\n", t->str);
    exit(1);
    return NULL;
}

Type* parse_type() {
    Type* base = parse_base_type();
    while (match(TK_STAR)) {
        consume_tok();
        base = add_type(TY_PTR, 4, base, NULL, NULL, 0);
    }
    return base;
}

// ---------- STRUCT STORAGE ----------
Type* structs[64];
int nstructs = 0;

// ---------- SYMBOLS ----------
typedef struct {
    char* name;
    int slot;
    Type* type;
} Var;
Var vars[256]; int var_count = 0;

int add_var(const char* name, Type* type) {
    int idx = var_count++;
    char buf[128];
    snprintf(buf, sizeof(buf), "v_%s", name);
    vars[idx].name = my_strdup(buf);
    vars[idx].slot = idx;
    vars[idx].type = type;
    printf("[DEBUG] Variable %s (type=%d, size=%d)\n", vars[idx].name, type->kind, type->size);
    return idx;
}

int find_var(const char* name) {
    for (int i = 0; i < var_count; i++) {
        if (!strcmp(vars[i].name + 2, name) || !strcmp(vars[i].name, name)) {
            printf("[DEBUG] Found variable: %s (size=%d)\n", vars[i].name, vars[i].type->size);
            return i;
        }
    }
    return -1;
}

// ---------- EXPRESSIONS ----------
typedef enum {
    EXPR_NUMBER, EXPR_VAR, EXPR_ADD, EXPR_SUB, EXPR_MUL, EXPR_DIV, EXPR_ADDR, EXPR_DEREF,
    EXPR_CMP_EQ, EXPR_CMP_NEQ, EXPR_CMP_LT, EXPR_CMP_GT, EXPR_CMP_LE, EXPR_CMP_GE,
    EXPR_INC, EXPR_DEC, EXPR_CALL, EXPR_FIELD,
    // *** NEW: BITWISE ***
    EXPR_SHL, EXPR_SHR, EXPR_OR, EXPR_AND
} ExprKind;

typedef struct Expr {
    ExprKind kind; long val; char* name; struct Expr* left; struct Expr* right;
    struct Type* type; int offset;
    struct Expr** args; int nargs;
} Expr;

// ---------- FORWARD DECLARATIONS ***
Expr* parse_primary();
Expr* parse_factor();
Expr* parse_term();
Expr* parse_comparison();
Expr* parse_bitwise();
Expr* parse_expr();
void debug_expr(Expr* e, int depth);
void parse_statement();
void emit_expr(Expr* e);
void emit_addr(Expr* e);

// *** NEW: STRUCT INITIALIZATION PARSING ***
void parse_struct_init(Type* stype, int slot) {
    expect(TK_LBRACE);
    int field_idx = 0;
    while (!match(TK_RBRACE)) {
        if (field_idx >= stype->nfields) {
            fprintf(stderr, "too many initializers for struct %s\n", stype->name);
            exit(1);
        }
        Field* field = stype->fields[field_idx];
        Expr* init_val = parse_expr();

        // Generate code to initialize this field
        fprintf(out, "    ; init %s.%s\n", vars[slot].name, field->name);
        emit_expr(init_val);
        fprintf(out, "    push eax\n");
        fprintf(out, "    lea eax,[%s]\n", vars[slot].name);
        fprintf(out, "    add eax,%d\n", field->offset);
        fprintf(out, "    pop ebx\n");
        int fs = field->type->size;
        if (fs == 1) fprintf(out, "    mov [eax],bl\n");
        else if (fs == 2) fprintf(out, "    mov [eax],bx\n");
        else fprintf(out, "    mov [eax],ebx\n");

        field_idx++;
        if (match(TK_COMMA)) consume_tok();
    }
    expect(TK_RBRACE);
    printf("[DEBUG] Initialized struct %s with %d fields\n", stype->name, field_idx);
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

Expr* parse_primary() {
    Token* t = peek_tok();
    if (match(TK_AMP)) {
        consume_tok();
        Expr* inner = parse_primary();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_ADDR;
        e->left = inner;
        e->type = add_type(TY_PTR, 4, inner->type, NULL, NULL, 0);
        printf("[DEBUG] Parsed ADDR (size=%d)\n", e->type->size);
        return e;
    }
    if (match(TK_STAR)) {
        consume_tok();
        Expr* inner = parse_primary();
        if (inner->type->kind != TY_PTR) { fprintf(stderr, "dereference non-pointer\n"); exit(1); }
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_DEREF;
        e->left = inner;
        e->type = inner->type->ptr_to;
        printf("[DEBUG] Parsed DEREF (size=%d)\n", e->type->size);
        return e;
    }
    if (match(TK_NUMBER)) {
        Token* n = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_NUMBER;
        e->val = n->val;
        e->type = ty_int;
        printf("[DEBUG] Parsed NUMBER(%ld, size=%d)\n", e->val, e->type->size);
        return e;
    }
    if (match(TK_CHAR_LITERAL)) {
        Token* n = consume_tok();
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_NUMBER;
        e->val = n->val;
        e->type = ty_char;
        printf("[DEBUG] Parsed CHAR(%ld, size=%d)\n", e->val, e->type->size);
        return e;
    }
    if (match(TK_IDENT)) {
        Token* id = consume_tok();
        char* name = my_strdup(id->str);
        if (match(TK_LPAREN)) {
            consume_tok(); // consume '('
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = EXPR_CALL;
            e->name = name;
            e->nargs = 0;
            e->args = NULL;
            if (!match(TK_RPAREN)) {
                do {
                    Expr* arg = parse_expr();
                    e->args = realloc(e->args, sizeof(Expr*) * (e->nargs + 1));
                    e->args[e->nargs++] = arg;
                } while (match(TK_COMMA) && (consume_tok(), 1));
            }
            expect(TK_RPAREN);
            e->type = ty_int; // assume int return
            printf("[DEBUG] Parsed CALL %s with %d args\n", e->name, e->nargs);
            return e;
        }
        else if (match(TK_DOT) || match(TK_ARROW)) {
            TokenKind op = consume_tok()->kind;
            if (!match(TK_IDENT)) { fprintf(stderr, "expected field name\n"); exit(1); }
            char* field_name = my_strdup(consume_tok()->str);
            int slot = find_var(name);
            if (slot < 0) { fprintf(stderr, "undefined variable %s\n", name); exit(1); }
            Type* base_type = vars[slot].type;
            ExprKind base_kind = (op == TK_DOT) ? EXPR_VAR : EXPR_DEREF;
            if (op == TK_ARROW) {
                if (base_type->kind != TY_PTR) { fprintf(stderr, "%s is not a pointer\n", name); exit(1); }
                base_type = base_type->ptr_to;
            }
            if (base_type->kind != TY_STRUCT) { fprintf(stderr, "%s is not a struct\n", name); exit(1); }
            int fidx = -1;
            for (int i = 0; i < base_type->nfields; i++) {
                if (!strcmp(base_type->fields[i]->name, field_name)) {
                    fidx = i;
                    break;
                }
            }
            if (fidx < 0) { fprintf(stderr, "no field %s in struct\n", field_name); exit(1); }
            Expr* base = calloc(1, sizeof(Expr));
            base->kind = base_kind;
            base->name = name;
            base->type = base_type;
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = EXPR_FIELD;
            e->left = base;
            e->offset = base_type->fields[fidx]->offset;
            e->type = base_type->fields[fidx]->type;
            printf("[DEBUG] Parsed FIELD %s (offset=%d, size=%d)\n", field_name, e->offset, e->type->size);
            return e;
        }
        // Simple variable
        int slot = find_var(name);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", name); exit(1); }
        Expr* e = calloc(1, sizeof(Expr));
        e->kind = EXPR_VAR;
        e->name = name;
        e->type = vars[slot].type;
        printf("[DEBUG] Parsed VAR(%s, size=%d)\n", e->name, e->type->size);
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
        e->type = vars[slot].type;
        printf("[DEBUG] Parsed %s(%s, size=%d)\n", (op == TK_INC) ? "INC" : "DEC", e->name, e->type->size);
        return e;
    }
    fprintf(stderr, "unexpected token in primary: kind=%d\n", t->kind);
    exit(1);
}

void debug_expr(Expr* e, int depth) {
    if (!e) return;
    for (int i = 0; i < depth; i++) printf("  ");
    switch (e->kind) {
    case EXPR_NUMBER: printf("NUMBER(%ld, size=%d)\n", e->val, e->type->size); break;
    case EXPR_VAR: printf("VAR(%s, size=%d)\n", e->name, e->type->size); break;
    case EXPR_ADDR: printf("ADDR(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); break;
    case EXPR_DEREF: printf("DEREF(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); break;
    case EXPR_ADD: printf("ADD(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_SUB: printf("SUB(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_MUL: printf("MUL(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_DIV: printf("DIV(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_INC: printf("INC(%s, size=%d)\n", e->name, e->type->size); break;
    case EXPR_DEC: printf("DEC(%s, size=%d)\n", e->name, e->type->size); break;
    case EXPR_CALL:
        printf("CALL %s (nargs=%d)\n", e->name, e->nargs);
        for (int i = 0; i < e->nargs; i++) debug_expr(e->args[i], depth + 1);
        break;
    case EXPR_CMP_EQ: printf("CMP_EQ(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_NEQ: printf("CMP_NEQ(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LT: printf("CMP_LT(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GT: printf("CMP_GT(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_LE: printf("CMP_LE(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_CMP_GE: printf("CMP_GE(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_FIELD: printf("FIELD(offset=%d, size=%d)\n", e->offset, e->type->size); debug_expr(e->left, depth + 1); break;
        // *** NEW: BITWISE ***
    case EXPR_SHL: printf("SHL(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_SHR: printf("SHR(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_OR:  printf("OR(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    case EXPR_AND: printf("AND(size=%d)\n", e->type->size); debug_expr(e->left, depth + 1); debug_expr(e->right, depth + 1); break;
    }
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
        n->type = ty_int; // assume
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_STAR) ? "MUL" : "DIV", n->type->size);
        e = n;
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
        n->type = ty_int; // assume
        printf("[DEBUG] Parsed %s(size=%d)\n", (op == TK_PLUS) ? "ADD" : "SUB", n->type->size);
        e = n;
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
        n->type = ty_int;
        switch (op) {
        case TK_EQ: n->kind = EXPR_CMP_EQ; printf("[DEBUG] Parsed CMP_EQ(size=%d)\n", n->type->size); break;
        case TK_NEQ: n->kind = EXPR_CMP_NEQ; printf("[DEBUG] Parsed CMP_NEQ(size=%d)\n", n->type->size); break;
        case TK_LT: n->kind = EXPR_CMP_LT; printf("[DEBUG] Parsed CMP_LT(size=%d)\n", n->type->size); break;
        case TK_GT: n->kind = EXPR_CMP_GT; printf("[DEBUG] Parsed CMP_GT(size=%d)\n", n->type->size); break;
        case TK_LE: n->kind = EXPR_CMP_LE; printf("[DEBUG] Parsed CMP_LE(size=%d)\n", n->type->size); break;
        case TK_GE: n->kind = EXPR_CMP_GE; printf("[DEBUG] Parsed CMP_GE(size=%d)\n", n->type->size); break;
        }
        e = n;
    }
    return e;
}

// *** NEW: BITWISE EXPRESSIONS ***
Expr* parse_bitwise() {
    Expr* e = parse_comparison();
    while (match(TK_SHL) || match(TK_SHR) || match(TK_OR) || match(TK_AND)) {
        TokenKind op = consume_tok()->kind;
        Expr* r = parse_comparison();
        Expr* n = calloc(1, sizeof(Expr));
        n->left = e;
        n->right = r;
        n->type = ty_int;
        switch (op) {
        case TK_SHL: n->kind = EXPR_SHL; printf("[DEBUG] Parsed SHL\n"); break;
        case TK_SHR: n->kind = EXPR_SHR; printf("[DEBUG] Parsed SHR\n"); break;
        case TK_OR:  n->kind = EXPR_OR;  printf("[DEBUG] Parsed OR\n");  break;
        case TK_AND: n->kind = EXPR_AND; printf("[DEBUG] Parsed AND\n"); break;
        }
        e = n;
    }
    return e;
}

Expr* parse_expr() {
    Expr* e = parse_bitwise();
    printf("[DEBUG] Expression tree:\n");
    debug_expr(e, 0);
    return e;
}

void emit_addr(Expr* e) {
    switch (e->kind) {
    case EXPR_VAR: {
        int slot = find_var(e->name);
        fprintf(out, "    lea eax,[%s]\n", vars[slot].name);
        break;
    }
    case EXPR_DEREF: {
        emit_expr(e->left);
        break;
    }
    case EXPR_FIELD: {
        // recursive addr
        emit_addr(e->left);
        if (e->offset) fprintf(out, "    add eax,%d\n", e->offset);
        break;
    }
    default: fprintf(stderr, "cannot take address\n"); exit(1);
    }
}

void emit_expr(Expr* e) {
    if (!e) return;
    printf("[DEBUG] Emitting code for expression kind: %d (size=%d)\n", e->kind, e->type->size);
    switch (e->kind) {
    case EXPR_NUMBER:
        fprintf(out, "    mov eax,%ld\n", e->val);
        break;
    case EXPR_VAR: {
        int slot = find_var(e->name);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", e->name); exit(1); }
        const char* varname = vars[slot].name;
        int s = e->type->size;
        if (s == 1) {
            fprintf(out, "    mov al,[%s]\n", varname);
            fprintf(out, "    movzx eax,al\n");
        }
        else if (s == 2) {
            fprintf(out, "    mov ax,[%s]\n", varname);
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    mov eax,[%s]\n", varname);
        }
        break;
    }
    case EXPR_DEREF: {
        emit_expr(e->left);
        int s = e->type->size;
        if (s == 1) {
            fprintf(out, "    mov al,[eax]\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (s == 2) {
            fprintf(out, "    mov ax,[eax]\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    mov eax,[eax]\n");
        }
        break;
    }
    case EXPR_ADDR: {
        emit_addr(e->left);
        break;
    }
    case EXPR_FIELD: {
        if (e->left->kind == EXPR_VAR) {
            int slot = find_var(e->left->name);
            fprintf(out, "    lea eax,[%s]\n", vars[slot].name);
        }
        else if (e->left->kind == EXPR_DEREF) {
            int slot = find_var(e->left->name);
            fprintf(out, "    mov eax,[%s]\n", vars[slot].name);
        }
        else {
            fprintf(stderr, "unsupported field base\n"); exit(1);
        }
        if (e->offset) fprintf(out, "    add eax,%d\n", e->offset);
        int s = e->type->size;
        if (s == 1) {
            fprintf(out, "    mov al,[eax]\n");
            fprintf(out, "    movzx eax,al\n");
        }
        else if (s == 2) {
            fprintf(out, "    mov ax,[eax]\n");
            fprintf(out, "    movzx eax,ax\n");
        }
        else {
            fprintf(out, "    mov eax,[eax]\n");
        }
        break;
    }
    case EXPR_INC: case EXPR_DEC: {
        int slot = find_var(e->name);
        if (slot < 0) { fprintf(stderr, "undefined variable %s\n", e->name); exit(1); }
        const char* varname = vars[slot].name;
        int s = e->type->size;
        if (s == 1) {
            fprintf(out, "    mov al,[%s]\n", varname);
            if (e->kind == EXPR_INC) fprintf(out, "    inc al\n");
            else fprintf(out, "    dec al\n");
            fprintf(out, "    mov [%s],al\n", varname);
            fprintf(out, "    movzx eax,al\n");
        }
        else if (s == 2) {
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
    case EXPR_ADD: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    add eax,ebx\n");
        break;
    }
    case EXPR_SUB: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    sub eax,ebx\n");
        break;
    }
    case EXPR_MUL: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    imul eax,ebx\n");
        break;
    }
    case EXPR_DIV: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    mov edx,0\n    idiv ebx\n");
        break;
    }
                 // *** NEW: BITWISE OPERATIONS ***
    case EXPR_SHL: {
        emit_expr(e->right); // shift amount
        fprintf(out, "    push eax\n");
        emit_expr(e->left);  // value to shift
        fprintf(out, "    pop ecx\n");
        fprintf(out, "    shl eax, cl\n");  // shift eax by cl (low byte of ecx)
        break;
    }
    case EXPR_SHR: {
        emit_expr(e->right); // shift amount
        fprintf(out, "    push eax\n");
        emit_expr(e->left);  // value to shift
        fprintf(out, "    pop ecx\n");
        fprintf(out, "    shr eax, cl\n");  // shift eax by cl
        break;
    }
    case EXPR_OR: {
        emit_expr(e->left);  fprintf(out, "    push eax\n");
        emit_expr(e->right); fprintf(out, "    pop ebx\n    or eax, ebx\n");
        break;
    }
    case EXPR_AND: {
        emit_expr(e->left);  fprintf(out, "    push eax\n");
        emit_expr(e->right); fprintf(out, "    pop ebx\n    and eax, ebx\n");
        break;
    }
    case EXPR_CMP_EQ: case EXPR_CMP_NEQ: case EXPR_CMP_LT: case EXPR_CMP_GT: case EXPR_CMP_LE: case EXPR_CMP_GE: {
        emit_expr(e->left);
        fprintf(out, "    push eax\n");
        emit_expr(e->right);
        fprintf(out, "    pop ebx\n");
        fprintf(out, "    cmp eax,ebx\n");
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
    Expr* cond = parse_expr(); expect(TK_RPAREN);

    emit_expr(cond);
    fprintf(out, "    test eax,eax\n");
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
    Expr* cond = parse_expr();
    expect(TK_RPAREN);
    int lbl = label_id++;
    emit_expr(cond);
    fprintf(out, "    test eax,eax\n");
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

// ---------- STRUCT DEFINITION ----------
void parse_struct_def() {
    consume_tok(); // struct
    if (!match(TK_IDENT)) { fprintf(stderr, "expected struct name\n"); exit(1); }
    char* name = my_strdup(consume_tok()->str);
    expect(TK_LBRACE);
    Type* st = add_type(TY_STRUCT, 0, NULL, name, NULL, 0);
    structs[nstructs++] = st;
    int offset = 0;
    while (!match(TK_RBRACE)) {
        Type* ftype = parse_type();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected field name\n"); exit(1); }
        char* fname = my_strdup(consume_tok()->str);
        expect(TK_SEMI);
        Field* f = malloc(sizeof(Field));
        f->name = fname;
        f->type = ftype;
        f->offset = offset;
        offset += ftype->size;
        st->fields = realloc(st->fields, sizeof(Field*) * (st->nfields + 1));
        st->fields[st->nfields++] = f;
    }
    st->size = offset;
    expect(TK_RBRACE);
    if (match(TK_SEMI)) consume_tok();
    printf("[DEBUG] Defined struct %s (size=%d)\n", name, st->size);
}

// ---------- INLINE ASM SUPPORT ----------
void parse_inline_asm() {
    consume_tok(); // __asm__
    expect(TK_LPAREN);
    if (!match(TK_STRING)) { fprintf(stderr, "expected asm string\n"); exit(1); }
    char* asm_str = consume_tok()->str;

    // *** EMIT ASM ***
    fprintf(out, "    ; inline asm: %s\n", asm_str);
    fprintf(out, "    %s\n", asm_str);

    expect(TK_RPAREN);
    expect(TK_SEMI);
    printf("[DEBUG] Emitted inline asm\n");
}

// ---------- STATEMENTS ----------
void parse_struct_field_assign() {
    // Already consumed IDENT (struct var), DOT, IDENT (field)
    char* struct_name = my_strdup(tokens[tok_pos - 3].str);
    char* field_name = my_strdup(tokens[tok_pos - 1].str);

    if (!match(TK_ASSIGN)) { fprintf(stderr, "expected = after field\n"); exit(1); }
    consume_tok();
    Expr* right = parse_expr();

    int slot = find_var(struct_name);
    if (slot < 0) { fprintf(stderr, "undefined %s\n", struct_name); exit(1); }
    Type* base_type = vars[slot].type;
    if (base_type->kind != TY_STRUCT) { fprintf(stderr, "%s is not a struct\n", struct_name); exit(1); }

    int foffset = -1;
    Type* ftype = NULL;
    for (int i = 0; i < base_type->nfields; i++) {
        if (!strcmp(base_type->fields[i]->name, field_name)) {
            foffset = base_type->fields[i]->offset;
            ftype = base_type->fields[i]->type;
            break;
        }
    }
    if (foffset < 0) { fprintf(stderr, "no field %s in struct\n", field_name); exit(1); }

    fprintf(out, "    ; assign to %s.%s\n", struct_name, field_name);
    emit_expr(right);
    fprintf(out, "    push eax\n");
    fprintf(out, "    lea eax,[%s]\n", vars[slot].name);
    if (foffset) fprintf(out, "    add eax,%d\n", foffset);
    fprintf(out, "    pop ebx\n");
    int fs = ftype->size;
    if (fs == 1) fprintf(out, "    mov [eax],bl\n");
    else if (fs == 2) fprintf(out, "    mov [eax],bx\n");
    else fprintf(out, "    mov [eax],ebx\n");

    expect(TK_SEMI);
}

// ----- FIXED: STATEMENTS WITH STRUCT INITIALIZATION ----------
void parse_statement() {
    Token* t = peek_tok();

    // *** 1. PRIMITIVE TYPE DECLARATION ***
    if (match(TK_INT) || match(TK_INT8) || match(TK_INT16) || match(TK_CHAR) || match(TK_VOID)) {
        Type* vtype = parse_type();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier\n"); exit(1); }
        Token* id = consume_tok();
        int slot = add_var(id->str, vtype);
        if (match(TK_ASSIGN)) {
            consume_tok();
            Expr* e = parse_expr();
            fprintf(out, "    ; assign to %s\n", vars[slot].name);
            emit_expr(e);
            int s = vtype->size;
            if (s == 1) fprintf(out, "    mov [%s],al\n", vars[slot].name);
            else if (s == 2) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
            else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
        }
        expect(TK_SEMI);
        return;
    }

    // *** 2. STRUCT TYPE DECLARATION WITH INITIALIZATION - FIXED! ***
    if (t->kind == TK_IDENT) {
        // Check if this is a STRUCT TYPE
        Type* stype = find_type(t->str);
        if (stype && stype->kind == TY_STRUCT) {
            consume_tok(); // consume struct type name (VGA_Char)
            if (!match(TK_IDENT)) { fprintf(stderr, "expected struct variable name\n"); exit(1); }
            Token* id = consume_tok(); // consume variable name (ch)
            int slot = add_var(id->str, stype);
            printf("[DEBUG] Declared struct variable %s (type=%s, size=%d)\n",
                id->str, stype->name, stype->size);

            if (match(TK_ASSIGN)) {
                consume_tok();
                parse_struct_init(stype, slot);  // *** HANDLE INITIALIZATION ***
            }
            expect(TK_SEMI);
            return;
        }
    }

    // *** 3. RETURN STATEMENT ***
    if (match(TK_RETURN)) {
        consume_tok();
        Expr* e = parse_expr();
        fprintf(out, "    ; return\n");
        emit_expr(e);
        fprintf(out, "    ret\n");
        has_return = 1;
        expect(TK_SEMI);
        return;
    }

    // *** 4. POINTER ASSIGNMENT *ptr = expr ***
    if (match(TK_STAR)) {
        consume_tok();
        if (!match(TK_IDENT)) { fprintf(stderr, "expected identifier after *\n"); exit(1); }
        Token* id = consume_tok();
        int slot = find_var(id->str);
        if (slot < 0 || vars[slot].type->kind != TY_PTR) { fprintf(stderr, "%s is not a pointer\n", id->str); exit(1); }
        if (!match(TK_ASSIGN)) { fprintf(stderr, "expected = after *%s\n", id->str); exit(1); }
        consume_tok();
        Expr* e = parse_expr();
        fprintf(out, "    ; assign to *%s\n", vars[slot].name);
        emit_expr(e);
        fprintf(out, "    push eax\n");
        fprintf(out, "    mov eax,[%s]\n", vars[slot].name);
        fprintf(out, "    pop ebx\n");
        int ts = vars[slot].type->ptr_to->size;
        if (ts == 1) fprintf(out, "    mov [eax],bl\n");
        else if (ts == 2) fprintf(out, "    mov [eax],bx\n");
        else fprintf(out, "    mov [eax],ebx\n");
        expect(TK_SEMI);
        return;
    }

    // *** 5. INLINE ASM ***
    if (match(TK_ASM)) {
        parse_inline_asm();
        return;
    }

    // *** 6. IDENT STARTS HERE - VARIABLE ASSIGN, STRUCT FIELD, CALL, INC/DEC ***
    if (match(TK_IDENT)) {
        Token* idtok = consume_tok();

        // *** STRUCT.FIELD ACCESS ***
        if (match(TK_DOT)) {
            consume_tok(); // dot
            if (!match(TK_IDENT)) { fprintf(stderr, "expected field name\n"); exit(1); }
            consume_tok(); // field name
            parse_struct_field_assign();
            return;
        }

        // *** NORMAL VARIABLE ASSIGNMENT ***
        int slot = find_var(idtok->str);
        if (slot >= 0 && match(TK_ASSIGN)) {
            consume_tok();
            Expr* e = parse_expr();
            fprintf(out, "    ; assign to %s\n", vars[slot].name);
            emit_expr(e);
            int s = vars[slot].type->size;
            if (s == 1) fprintf(out, "    mov [%s],al\n", vars[slot].name);
            else if (s == 2) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
            else fprintf(out, "    mov [%s],eax\n", vars[slot].name);
            expect(TK_SEMI);
            return;
        }

        // *** INC/DEC ***
        if (slot >= 0 && (match(TK_INC) || match(TK_DEC))) {
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = (peek_tok()->kind == TK_INC) ? EXPR_INC : EXPR_DEC;
            consume_tok();
            e->name = my_strdup(idtok->str);
            e->type = vars[slot].type;
            fprintf(out, "    ; %s %s\n", (e->kind == EXPR_INC) ? "inc" : "dec", vars[slot].name);
            emit_expr(e);
            expect(TK_SEMI);
            return;
        }

        // *** FUNCTION CALL ***
        if (match(TK_LPAREN)) {
            Expr* e = calloc(1, sizeof(Expr));
            e->kind = EXPR_CALL;
            e->name = my_strdup(idtok->str);
            e->nargs = 0;
            e->args = NULL;
            consume_tok();
            if (!match(TK_RPAREN)) {
                do {
                    Expr* arg = parse_expr();
                    e->args = realloc(e->args, sizeof(Expr*) * (e->nargs + 1));
                    e->args[e->nargs++] = arg;
                } while (match(TK_COMMA) && (consume_tok(), 1));
            }
            expect(TK_RPAREN);
            fprintf(out, "    ; call %s\n", e->name);
            emit_expr(e);
            expect(TK_SEMI);
            return;
        }

        fprintf(stderr, "unexpected pattern for ident '%s'\n", idtok->str);
        exit(1);
    }

    // *** 7. CONTROL FLOW ***
    if (match(TK_IF)) { parse_if(); return; }
    if (match(TK_WHILE)) { parse_while(); return; }

    // *** 8. PRINT ***
    if (match(TK_PRINT)) {
        consume_tok();
        expect(TK_LPAREN);
        if (!match(TK_STRING)) { fprintf(stderr, "print expects string literal\n"); exit(1); }
        Token* fmt_tok = consume_tok();
        const char* fmt = fmt_tok->str;
        const char* label = add_string(fmt);
        int arg_count = 0;
        Expr* args[8];

        if (match(TK_COMMA)) {
            do {
                consume_tok();  // eat comma
                args[arg_count++] = parse_expr();
            } while (match(TK_COMMA));
        }
        expect(TK_RPAREN);
        expect(TK_SEMI);

        // *** FIXED: CORRECT STACK ORDER (RIGHT TO LEFT) ***
        for (int i = arg_count - 1; i >= 0; --i) {
            emit_expr(args[i]);
            fprintf(out, "    push eax\n");
        }
        fprintf(out, "    lea eax,[%s]\n", label);
        fprintf(out, "    push eax\n");
        fprintf(out, "    call print_fmt\n");
        fprintf(out, "    add esp,%d\n", (arg_count + 1) * 4);
        printf("[DEBUG] PRINT: %d args\n", arg_count);
        return;
    }

    fprintf(stderr, "unexpected statement token: kind=%d\n", t->kind);
    consume_tok();
}

// ---------- FIXED: FULL FUNCTION SUPPORT ----------
void parse_function_and_emit() {
    has_return = 0;  // *** RESET for each function ***

    Type* ret_type = parse_type();

    if (!match(TK_IDENT)) {
        fprintf(stderr, "expected function name\n"); exit(1);
    }
    Token* func_name = consume_tok();
    const char* name = my_strdup(func_name->str);
    printf("[DEBUG] Parsing function: %s (returns size=%d)\n", name, ret_type->size);

    expect(TK_LPAREN);

    char* param_names[32];
    Type* param_types[32];
    int param_count = 0;

    if (!match(TK_RPAREN)) {
        do {
            param_types[param_count] = parse_type();
            if (!match(TK_IDENT)) {
                fprintf(stderr, "expected param name\n"); exit(1);
            }
            Token* pid = consume_tok();
            param_names[param_count] = my_strdup(pid->str);
            add_var(param_names[param_count], param_types[param_count]);
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
        int s = param_types[i]->size;
        if (s == 1) fprintf(out, "    mov [%s],al\n", vars[slot].name);
        else if (s == 2) fprintf(out, "    mov [%s],ax\n", vars[slot].name);
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
        fprintf(out, "%s resb %d\n", vars[i].name, vars[i].type->size);
    }
    fprintf(out, "vga_cursor resd 1\n");
}

// *** ENHANCED: PRINT ROUTINE WITH %c & %x ***
void emit_print_routine() {
    fprintf(out,
        "\nprint_string:\n"
        "    pusha\n"
        "    mov esi, [esp+32]\n"
        "    mov edi, 0xB8000\n"
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
        "    mov esi, [ebp+8]\n"
        "    mov edi, 0xB8000\n"
        "    lea ebx, [ebp+12]\n"
        ".loop:\n"
        "    lodsb\n"
        "    test al, al\n"
        "    jz .done\n"
        "    cmp al, 10\n"              // *** NEW: Skip newline ***
        "    je .loop\n"                 // *** NEW: Skip newline ***
        "    cmp al, '%%'\n"
        "    jne .print\n"
        "    lodsb\n"
        "    cmp al, 'c'\n"
        "    je .char\n"
        "    cmp al, 'd'\n"
        "    je .decimal\n"
        "    cmp al, 'x'\n"
        "    je .hex\n"
        "    mov al, '?'\n"
        ".print:\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    jmp .loop\n"
        ".char:\n"
        "    mov al, [ebx]\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    add ebx, 4\n"
        "    jmp .loop\n"
        ".decimal:\n"
        "    mov eax, [ebx]\n"
        "    call print_int\n"
        "    add ebx, 4\n"
        "    jmp .loop\n"
        ".hex:\n"
        "    mov eax, [ebx]\n"
        "    call print_hex8\n"
        "    add ebx, 4\n"
        "    jmp .loop\n"
        ".done:\n"
        "    pop edi\n"
        "    pop esi\n"
        "    pop ebx\n"
        "    pop ebp\n"
        "    ret\n\n"

        "\nprint_int:\n"
        "    push ebx\n"
        "    push ecx\n"
        "    push edx\n"
        "    test eax, eax\n"
        "    jns .pos\n"
        "    push eax\n"
        "    mov al, '-'\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    pop eax\n"
        "    neg eax\n"
        ".pos:\n"
        "    mov ebx, 10\n"
        "    xor ecx, ecx\n"
        ".div:\n"
        "    xor edx, edx\n"
        "    div ebx\n"
        "    push edx\n"
        "    inc ecx\n"
        "    test eax, eax\n"
        "    jnz .div\n"
        ".print:\n"
        "    pop eax\n"
        "    add al, '0'\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    loop .print\n"
        "    pop edx\n"
        "    pop ecx\n"
        "    pop ebx\n"
        "    ret\n\n"

        "\nprint_hex8:\n"
        "    push ebx\n"
        "    push ecx\n"
        "    push edx\n"
        "    mov ecx, 8\n"
        ".hloop:\n"
        "    rol eax, 4\n"
        "    mov ebx, eax\n"
        "    and ebx, 0xF\n"
        "    mov dl, [hex_chars + ebx]\n"
        "    push eax\n"
        "    mov al, dl\n"
        "    mov ah, 0x0F\n"
        "    stosw\n"
        "    pop eax\n"
        "    loop .hloop\n"
        "    pop edx\n"
        "    pop ecx\n"
        "    pop ebx\n"
        "    ret\n\n"

        "\nhex_chars db '0123456789ABCDEF'\n"
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

    init_types();
    tokenize();

    printf("[*] Opening output file: %s\n", argv[2]);
    out = fopen(argv[2], "w");
    if (!out) { perror("fopen output"); free(src); return 1; }

    fprintf(out, "[BITS 32]\n\n[org 0x1000]\n\nsection .text\nglobal kernel_main\n\n");
    fprintf(out, "    jmp kernel_main\n\n");

    printf("[*] Parsing program...\n");
    while (!match(TK_EOF)) {
        if (match(TK_STRUCT)) {
            parse_struct_def();
        }
        else {
            parse_function_and_emit();
        }
    }

    emit_print_routine();
    emit_data_section();
    emit_bss_section();

    printf("[*] Done. Assembly written to %s\n", argv[2]);

    fclose(out);
    free(src);
    return 0;
}