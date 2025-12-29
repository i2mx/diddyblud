#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// start of lexing code

struct String {
    const char* data;
    size_t len;
};

struct String make_string(const char* s)
{
    return (struct String) { s, strlen(s) };
}

bool s_is_s(struct String s1, const char* s2)
{
    return strncmp(s1.data, s2, s1.len) == 0 && s1.len == strlen(s2);
}

struct SourcePos {
    int line;
    int column;
    size_t offset;
};

struct SourceRange {
    const char* filename;
    struct SourcePos start;
    struct SourcePos end;
};

enum TokenKind {
    TokenKindIllegal,
    TokenKindEof,
    TokenKindNat, // 0 or 013 or 123
    TokenKindIdent, // _hello_world1 or _123a
    TokenKindOperator, // +, - |> <? =>
    TokenKindLParen, // (
    TokenKindRParen, // )
    TokenKindLBracket, // [
    TokenKindRBracket, // ]
    TokenKindLBrace, // {
    TokenKindRBrace, // }
    TokenKindSemicolon, // ;
    TokenKindComma, // ,
};

// A token is either a known symbol like '('
// or a class of symbols like identifiers
struct Token {
    enum TokenKind kind;
    struct String lexeme;
    struct SourceRange range;
};

void print_token(struct Token token)
{
    const char* kind_name;

    switch (token.kind) {
    case TokenKindIllegal:
        kind_name = "Illegal";
        break;
    case TokenKindEof:
        kind_name = "EOF";
        break;
    case TokenKindIdent:
        kind_name = "Identifier";
        break;
    case TokenKindNat:
        kind_name = "Natural";
        break;
    case TokenKindOperator:
        kind_name = "Operator";
        break;
    case TokenKindRParen:
        kind_name = "RParen";
        break;
    case TokenKindLParen:
        kind_name = "LParen";
        break;
    case TokenKindRBracket:
        kind_name = "RBracket";
        break;
    case TokenKindLBracket:
        kind_name = "LBracket";
        break;
    case TokenKindRBrace:
        kind_name = "RBrace";
        break;
    case TokenKindLBrace:
        kind_name = "LBrace";
        break;
    case TokenKindSemicolon:
        kind_name = "Semicolon";
        break;
    case TokenKindComma:
        kind_name = "Comma";
        break;
    default:
        kind_name = "Unknown";
        break;
    }

    printf("[Token] Kind: %-12s | Lexeme: '%.*s' | Line: %d, Column: %d\n",
        kind_name, (int)token.lexeme.len, token.lexeme.data,
        token.range.start.line, token.range.start.column);
}

struct Lexer {
    struct String input;
    struct SourcePos pos;
};

struct Lexer* make_lexer(const char* input)
{
    struct Lexer* lexer = malloc(sizeof(struct Lexer));
    assert(lexer && "allocation should never fail");

    lexer->input.data = input;
    lexer->input.len = strlen(input);
    lexer->pos.column = 1;
    lexer->pos.line = 1;
    lexer->pos.offset = 0;
    return lexer;
}

char peek(struct Lexer* lexer)
{
    if (lexer->pos.offset >= (*lexer).input.len)
        return '\0';
    else
        return lexer->input.data[(*lexer).pos.offset];
}

void advance(struct Lexer* lexer)
{
    assert(lexer->pos.offset < lexer->input.len
        && "cannot advance past end of input");

    if (lexer->input.data[lexer->pos.offset] == '\n') {
        ++lexer->pos.line;
        lexer->pos.column = 1;
    } else {
        ++lexer->pos.column;
    }

    lexer->pos.offset++;
    return;
}

void skip_whitespace(struct Lexer* lexer)
{
    while (peek(lexer) == ' ')
        advance(lexer);
}

bool isspecial(char c)
{
    return (
        c == '!'
        || c == '$'
        || c == '%'
        || c == '&'
        || c == '*'
        || c == '+'
        || c == '-'
        || c == '.'
        || c == '/'
        || c == '<'
        || c == '='
        || c == '>'
        || c == '?'
        || c == '@'
        || c == '^'
        || c == '|'
        || c == '~');
}

bool isdelim(char c)
{
    return (
        c == '('
        || c == ')'
        || c == '['
        || c == ']'
        || c == '{'
        || c == '}'
        || c == ';'
        || c == ',');
}

enum TokenKind delim_kind(char c)
{
    switch (c) {
    case '(':
        return TokenKindLParen;
    case ')':
        return TokenKindRParen;
    case '[':
        return TokenKindLBracket;
    case ']':
        return TokenKindRBracket;
    case '{':
        return TokenKindLBrace;
    case '}':
        return TokenKindRBrace;
    case ';':
        return TokenKindSemicolon;
    case ',':
        return TokenKindComma;
    };
    return TokenKindIllegal;
}

struct Token next_token(struct Lexer* lexer)
{
    skip_whitespace(lexer);
    struct SourcePos start = (*lexer).pos;

    char c = peek(lexer);

    if (c == '\0') { // EOF
        return (struct Token) {
            .kind = TokenKindEof,
            .lexeme = { (*lexer).input.data + start.offset, 1 },
            .range = {
                .start = start,
                .end = start,
            }
        };
    }

    if (isdelim(c)) {
        advance(lexer);
        return (struct Token) {
            .kind = delim_kind(c),
            .lexeme = { (*lexer).input.data + start.offset, 1 },
            .range = {
                .start = start,
                .end = (*lexer).pos,
            }
        };
    }

    if (isdigit(c)) {
        while (isdigit(peek(lexer)) || peek(lexer) == '_') {
            advance(lexer);
        }

        return (struct Token) {
            .kind = TokenKindNat,
            .lexeme = {
                .data = (*lexer).input.data + start.offset,
                .len = (*lexer).pos.offset - start.offset,
            },
            .range = {
                .start = start,
                .end = (*lexer).pos,
            }
        };
    }

    if (isalpha(c)) {
        while (peek(lexer) == '_' || isalnum(peek(lexer))) {
            advance(lexer);
        }
        return (struct Token) {
            .kind = TokenKindIdent,
            .lexeme = {
                .data = (*lexer).input.data + start.offset,
                .len = (*lexer).pos.offset - start.offset,
            },
            .range = {
                .start = start,
                .end = (*lexer).pos,
            }
        };
    }

    if (isspecial(c)) {
        while (isspecial(peek(lexer))) {
            advance(lexer);
        }
        return (struct Token) {
            .kind = TokenKindOperator,
            .lexeme = {
                .data = lexer->input.data + start.offset,
                .len = lexer->pos.offset - start.offset,
            },
            .range = {
                .start = start,
                .end = lexer->pos,
            }
        };
    }

    fprintf(stderr, "[Lexer Error] found illegal character '%c' at %d:%d in %s",
        peek(lexer),
        start.line,
        start.column,
        NULL);
    exit(1);
}

struct Token peek_token(struct Lexer* lexer)
{
    struct SourcePos start = lexer->pos;
    struct Token t = next_token(lexer);
    lexer->pos = start;
    return t;
}
// end of lexing code

void test_lexer()
{
    struct Lexer* lexer = make_lexer("1 + 2 * 3");
    struct Token t;
    while ((t = next_token(lexer)).kind != TokenKindEof) {
        print_token(t);
    }
    free(lexer);
}

// start of expression parsing code
enum ExpressionKind {
    ExpressionKindAtom,
    ExpressionKindMonad,
    ExpressionKindDyad,
};

struct Expression {
    enum ExpressionKind kind;
    union {
        struct String atom;
        struct {
            struct String operation;
            struct Expression* expr;
        } monad;
        struct {
            struct String operation;
            struct Expression* left;
            struct Expression* right;
        } dyad;
    };
};

// returns true if something we fail to find the binding
// power of an infix operator
bool infix_binding_power(struct String op, int* lbp, int* rbp)
{
    if (s_is_s(op, "+") || s_is_s(op, "-")) {
        *lbp = 1;
        *rbp = 2;
        return false;
    }
    if (s_is_s(op, "*") || s_is_s(op, "/")) {
        *lbp = 3;
        *rbp = 4;
        return false;
    }
    return true;
}

// returns true if something we fail to find the binding
// power of an prefix operator
bool prefix_binding_power(struct String op, int* rbp)
{
    if (s_is_s(op, "+") || s_is_s(op, "-")) {
        *rbp = 6;
        return false;
    }
    return true;
}

// returns true if something we fail to find the binding
// power of an postfix operator
bool postfix_binding_power(struct String op, int* lbp)
{
    if (s_is_s(op, "!")) {
        *lbp = 7;
        return false;
    }
    return true;
}

void print_expression(struct Expression* expression)
{
    if (expression == NULL) {
        printf("NULL ");
        return;
    }

    switch (expression->kind) {
    case ExpressionKindAtom:
        printf("%.*s ", (int)expression->atom.len, expression->atom.data);
        break;
    case ExpressionKindMonad:
        print_expression(expression->monad.expr);
        printf("[%.*s] ", (int)expression->monad.operation.len,
            expression->monad.operation.data);
        break;
    case ExpressionKindDyad:
        print_expression(expression->dyad.left);
        print_expression(expression->dyad.right);
        printf("%.*s ", (int)expression->dyad.operation.len,
            expression->dyad.operation.data);
        break;
    }
}

// a null pointer is used to indicate an empty expression
struct Expression* expr_bp(struct Lexer* lexer, unsigned int minbp)
{
    struct Token t = next_token(lexer);

    struct Expression* lhs;
    if (t.kind == TokenKindIdent || t.kind == TokenKindNat) {
        // atoms
        lhs = malloc(sizeof(struct Expression));
        lhs->kind = ExpressionKindAtom;
        lhs->atom = t.lexeme;
    } else if (t.kind == TokenKindLParen) {
        // parenthesised expressions
        struct Token t2;
        lhs = expr_bp(lexer, 0);
        if (lhs == NULL) {
            fprintf(stderr,
                "[Parsing Error] missing expression after the open parenthesis '('"
                " at %d:%d in %s",
                t.range.start.line,
                t.range.start.column,
                t.range.filename);
            exit(1);
        }

        if ((t2 = next_token(lexer)).kind != TokenKindRParen) {
            fprintf(stderr,
                "[Parsing Error] expected matching closing parenthesis"
                " ')' for '(' at %d:%d to enclose expression"
                " instead found '%.*s' at %d:%d in %s ",
                t.range.start.line,
                t.range.start.column,
                (int)t2.lexeme.len,
                t2.lexeme.data,
                t2.range.start.line,
                t2.range.end.column,
                t.range.filename);
            exit(1);
        }
    } else if (t.kind == TokenKindOperator) {
        // prefix operators
        int rbp;
        if (prefix_binding_power(t.lexeme, &rbp) == 0) {
            struct Expression* rhs = expr_bp(lexer, rbp);
            if (rhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression on the right hand side"
                    " of the prefix operator '%.*s' after %d:%d in %s",
                    (int)t.lexeme.len,
                    t.lexeme.data,
                    t.range.end.line,
                    t.range.end.column,
                    t.range.filename);
                exit(1);
            }
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindMonad;
            lhs->monad.expr = rhs;
            lhs->monad.operation = t.lexeme;
        }
    } else {
        // failure
        return NULL;
    }

    // extend the expression as much as possible
    // using post and infix operators
    while (true) {
        struct Token t = peek_token(lexer);
        int lbp, rbp;

        // special post fixes
        if (t.kind == TokenKindLBracket) {
            // array access
            struct Token t2;
            next_token(lexer);
            struct Expression* rhs = expr_bp(lexer, rbp);

            if (rhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the open bracket '[' "
                    " at %d:%d in %s",
                    t.range.end.line,
                    t.range.end.column,
                    t.range.filename);
                exit(1);
            }
            if ((t2 = next_token(lexer)).kind != TokenKindRBracket) {
                fprintf(stderr,
                    "[Parsing Error] expected matching closing bracket ']'"
                    " for '[' at %d:%d for indexed access,"
                    " instead found '%.*s' at %d:%d in %s",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t2.range.start.line,
                    t2.range.end.column,
                    t.range.filename);
                exit(1);
            }

            struct Expression* new = malloc(sizeof(struct Expression));
            new->kind = ExpressionKindDyad;
            new->dyad.left = lhs;
            new->dyad.right = rhs;
            new->dyad.operation = make_string("[]");

            lhs = new;
            continue;

        } else if (t.kind == TokenKindLParen) {
            printf("Function call syntax has yet to be implemented");
            exit(1);
        }

        if (t.kind != TokenKindOperator)
            // no more way to extend the operator
            break;

        // first extend all the postfix operators possible then evaluate infixes
        // should try to design the language so that there is no cooked shi
        // when using postfix operators, i dont really like this but
        // it'll have to do because chaining is quite nice i guess

        if (postfix_binding_power(t.lexeme, &lbp) == 0) {
            // postfix operators
            if (lbp < minbp)
                break;
            next_token(lexer);

            // extend expression
            struct Expression* new = malloc(sizeof(struct Expression));
            new->kind = ExpressionKindMonad;
            new->monad.expr = lhs;
            new->monad.operation = t.lexeme;

            lhs = new;
            continue;
        }

        // finally handle infix
        if (infix_binding_power(t.lexeme, &lbp, &rbp)) {
            fprintf(stderr,
                "[Parsing Error] nonexistent operator '%.*s' at %d:%d in %s",
                (int)t.lexeme.len,
                t.lexeme.data,
                t.range.start.line,
                t.range.start.column,
                t.range.filename);
            exit(1);
        }

        if (lbp < minbp)
            break;

        next_token(lexer);
        // once we have taken an operator we need an expression on the right hand side
        struct Expression* rhs = expr_bp(lexer, rbp);
        if (rhs == NULL) {
            fprintf(stderr,
                "[Parsing Error] missing expression on the right hand side"
                " of the infix operator '%.*s' after %d:%d in %s",
                (int)t.lexeme.len,
                t.lexeme.data,
                t.range.end.line,
                t.range.end.column,
                t.range.filename);
            exit(1);
        }
        assert(rhs != NULL && "good luck");

        // combine into one expression
        struct Expression* new = malloc(sizeof(struct Expression));
        new->kind = ExpressionKindDyad;
        new->dyad.left = lhs;
        new->dyad.right = rhs;
        new->dyad.operation = t.lexeme;

        // move new expression to left and continue
        lhs = new;
    }

    return lhs;
}

struct Expression* expr(const char* input)
{
    struct Lexer* lexer = make_lexer(input);
    return expr_bp(lexer, 0);
}
// end of the expression parsing code
int main()
{
    // test_lexer();
    struct Expression* exp = expr("123[132] + atan[x] + 1_000_000");
    print_expression(exp);
}
