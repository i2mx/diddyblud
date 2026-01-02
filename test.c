#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// // start of ArrayList implementation

// TODO: test this shit

// // c sucks

#define ArrayList(T) ArrayList_##T

#define DEFINE_ARRAYLIST(T) \
    struct ArrayList(T) {   \
        size_t len;         \
        size_t cap;         \
        T* data;            \
    }

// there was a line here fixed by chatgpt which was uh
// i wrote sizeof(char) but it actually should be sizeof(T) so that's awesome
#define append(T, arrayList, x)                                               \
    do {                                                                      \
        if ((arrayList).len < (arrayList).cap) {                              \
            (arrayList).data[(arrayList).len++] = (x);                        \
        } else {                                                              \
            size_t newCap = (arrayList).cap ? (arrayList).cap * 2 : 1;        \
            T* newBuffer = malloc(sizeof(T) * newCap);                        \
            memcpy(newBuffer, (arrayList).data, sizeof(T) * (arrayList).len); \
            free((arrayList).data);                                           \
            (arrayList).data = newBuffer;                                     \
            (arrayList).cap = newCap;                                         \
            (arrayList).data[(arrayList).len++] = x;                          \
        }                                                                     \
    } while (0)

#define empty_list(T) \
    (struct ArrayList(T)) { .cap = 1, .len = 0, .data = malloc(sizeof(T)) }

#define free_list(arrayList) free((arrayList).data)

// // end of ArrayList implementation

// // start of lexing code

// a custom string type for describing slices of byte arrays /
// a string_view
struct String {
    const char* data;
    size_t len;
};

// makes a struct String out of a c-style string
static inline struct String make_string(const char* s)
{
    return (struct String) { s, strlen(s) };
}

// checks to see if a struct String is the same as a c-style string
static inline bool seq(struct String s1, const char* s2)
{
    return strncmp(s1.data, s2, s1.len) == 0 && s1.len == strlen(s2);
}

// a custom type to describe a current in a source code
struct SourcePos {
    int line;
    int column;
    size_t offset;
};

// describes a continuous section / range in source code
struct SourceRange {
    const char* filename;
    struct SourcePos start;
    struct SourcePos end;
};

// the kinds of tokens that my lexer can produce, try to keep this minimal for now
enum TokenKind {
    // TODO: expand out tokens so known tokens are a single enum item for example expand
    // keyword to TokenKindVal TokenKindIf and so on

    TokenKindIllegal,
    TokenKindEof,

    TokenKindNat, // Examples: 0, 013, 123

    TokenKindIdent, // Examples: hello_world1, a_2

    TokenKindKeyword, // Examples: let, val, if, then, else, while, do, end
                      //
                      // try to have as few of these are possible

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

// Lexigraphical elements that are produced by the lexer in lexigraphical analysis
// and consumed by the parser to produce an AST
struct Token {
    enum TokenKind kind;
    struct String lexeme;
    struct SourceRange range;
};

// FOR DEBUGGING
void print_token(const struct Token token)
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
    case TokenKindKeyword:
        kind_name = "Keyword";
        break;
    default:
        kind_name = "Unknown";
        break;
    }

    // I asked chatgpt to write this function and it's a little embarassing.
    printf("[Token] Kind: %-12s | Lexeme: %-8.*s | Line: %d, Column: %d\n",
        kind_name, (int)token.lexeme.len, token.lexeme.data,
        token.range.start.line, token.range.start.column);
}

// Describes the state of a lexer, such as how far along
// it is through the source code
//
// // TODO: some optimisation to do with storing
// the current and next_token so that peek_token is faster
struct Lexer {
    struct String input;
    struct SourcePos pos;
};

// Initializes the state of a lexer based on an input string.
struct Lexer* make_lexer(const char* input)
{
    // NOTE: we only ever make one lexer and it's very cheap
    // so this should never leak memoery

    // NOTE: this doesn't need to malloc at all?

    struct Lexer* lexer = malloc(sizeof(struct Lexer));
    assert(lexer && "allocation should never fail");

    lexer->input.data = input;
    lexer->input.len = strlen(input);
    lexer->pos.column = 1;
    lexer->pos.line = 1;
    lexer->pos.offset = 0;
    return lexer;
}

// A helper function which returns the character
// the lexer is currently up to and \0 if the
// position of the lexer exceeds the length of the string
char peek(struct Lexer* lexer)
{
    // TODO: I'm sure this check is unnecessary
    // if we are smart enough to not continue after
    // we reach EOF
    if (lexer->pos.offset >= (*lexer).input.len)
        return '\0';
    else
        return lexer->input.data[(*lexer).pos.offset];
}

// Updates the lexer state to move the the next character
// in the input string
void advance(struct Lexer* lexer)
{
    // TODO: maybe useless assert?
    assert(lexer->pos.offset < lexer->input.len
        && "cannot advance past end of input");

    // TODO: we usually skip whitespace and so
    // most of the time when we call advance we know to
    // just use the else branch but we are having to
    // do this check which can be a little annoying
    if (lexer->input.data[lexer->pos.offset] == '\n') {
        ++lexer->pos.line;
        lexer->pos.column = 1;
    } else {
        ++lexer->pos.column;
    }

    lexer->pos.offset++;
    return;
}

// advances the position of the lexer until we reach a
// non whitespace character,
static inline void skip_whitespace(struct Lexer* lexer)
{
    // the language will not be newline
    // or whitespace sensitive so we will skip these
    // however I have people who use tabs so we will not skip these
    // and then explicitly call out that a tab has been used.
    // NOTE: fuck \r
    while (peek(lexer) == ' ' || peek(lexer) == '\n' || peek(lexer) == '\r')
        advance(lexer);
}

// determines if a character belongs in an operator
static inline bool isspecial(char c)
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

// determines if a character is a "deliminator"
static inline bool isdelim(const char c)
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

// returns the exact deliminater type of a character
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

// takes the token at the lexer's current position and moves the position
// of the lexer forward
struct Token next_token(struct Lexer* lexer)
{
    skip_whitespace(lexer);
    struct SourcePos start = (*lexer).pos;

    char c = peek(lexer);

    if (c == '\0') {
        return (struct Token) {
            .kind = TokenKindEof,
            .lexeme = { (*lexer).input.data + start.offset, 1 },
            .range = {
                .start = start,
                .end = start,
            }
        };
    }

    if (c == ';') {
        advance(lexer);
        return (struct Token) {
            .kind = TokenKindSemicolon,
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
        struct String s = {
            .data = (*lexer).input.data + start.offset,
            .len = (*lexer).pos.offset - start.offset,
        };

        return (struct Token) {
            .kind = seq(s, "if")
                    || seq(s, "then")
                    || seq(s, "else")
                    || seq(s, "while")
                    || seq(s, "do")
                    || seq(s, "done")
                    || seq(s, "let")
                    || seq(s, "break")
                ? TokenKindKeyword
                : TokenKindIdent,
            .lexeme = s,
            .range = {
                .start = start,
                .end = (*lexer).pos,
            }
        };
    }

    // TODO: support backtick surrounded words to be used as operators
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

    fprintf(stderr, "[Lexer Error] found illegal character '%c' at %d:%d in %s \n",
        peek(lexer),
        start.line,
        start.column,
        NULL);
    exit(1);
}

// returns the token at the lexer's current position
struct Token peek_token(struct Lexer* lexer)
{
    // TODO: optimize
    struct SourcePos start = lexer->pos;
    struct Token t = next_token(lexer);
    lexer->pos = start;
    return t;
}

// // end of lexing code

// // start of expression parsing code

enum ExpressionKind {
    ExpressionKindAtom,
    ExpressionKindMonad,
    ExpressionKindDyad,
    ExpressionKindCall,
    ExpressionKindBranch,
    ExpressionKindWhile,
    ExpressionKindLet,
    ExpressionKindLambda,
    ExpressionKindBreak,
};

typedef struct Expression* Expression;
DEFINE_ARRAYLIST(Expression);

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
        struct {
            struct Expression* function;
            struct ArrayList(Expression) argument_list;
        } call;
        struct {
            struct Expression* condition;
            struct Expression* if_branch;
            struct Expression* else_branch;
        } branch;
        struct {
            struct Expression* condition;
            struct Expression* body;
        } while_loop;
        struct {
            struct String identifier;
            struct Expression* value;
            struct Expression* body;
        } let;
        struct {
            struct String identifier;
            struct Expression* body;
        } lambda;
        struct {
        } break_expr;
    };
};

// returns true if we fail to find the binding
// power of an prefix operator
bool prefix_binding_power(struct String op, int* rbp)
{
    if (seq(op, "+") || seq(op, "-") || seq(op, "!")) {
        *rbp = 19;
        return false;
    }
    if (seq(op, "if") || seq(op, "while") || seq(op, "let") || seq(op, "lambda")) {
        *rbp = 3;
        return false;
    }
    return true;
}

// returns true if we fail to find the binding
// power of an infix operator
bool infix_binding_power(struct String op, int* lbp, int* rbp)
{
    if (seq(op, ";")) {
        *lbp = 2;
        *rbp = 1;
        return false;
    }
    if (seq(op, "=")) {
        *lbp = 6;
        *rbp = 5;
        return false;
    }
    if (seq(op, "||")) {
        *lbp = 7;
        *rbp = 8;
        return false;
    }
    if (seq(op, "&&")) {
        *lbp = 9;
        *rbp = 10;
        return false;
    }
    if (seq(op, "==") || seq(op, "!=")) {
        *lbp = 11;
        *rbp = 12;
        return false;
    }
    if (seq(op, "<") || seq(op, "<=") || seq(op, ">") || seq(op, ">=")) {
        *lbp = 13;
        *rbp = 14;
        return false;
    }
    if (seq(op, "+") || seq(op, "-")) {
        *lbp = 15;
        *rbp = 16;
        return false;
    }
    if (seq(op, "*") || seq(op, "/") || seq(op, "%")) {
        *lbp = 17;
        *rbp = 18;
        return false;
    }
    return true;
}

// returns true if  we fail to find the binding
// power of an postfix operator
bool postfix_binding_power(struct String op, int* lbp)
{
    if (seq(op, "!")) {
        *lbp = 20;
        return false;
    }
    if (seq(op, "[") || seq(op, "(")) {
        *lbp = 20;
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
        printf("[1]%.*s ", (int)expression->monad.operation.len,
            expression->monad.operation.data);
        break;
    case ExpressionKindDyad:
        if (seq(expression->dyad.operation, ";")) {
            print_expression(expression->dyad.left);
            printf("; ");
            print_expression(expression->dyad.right);
        } else {
            print_expression(expression->dyad.left);
            print_expression(expression->dyad.right);
            printf("[2]%.*s ", (int)expression->dyad.operation.len,
                expression->dyad.operation.data);
        }
        break;
    case ExpressionKindCall:
        for (size_t i = 0; i < expression->call.argument_list.len; ++i) {
            print_expression(expression->call.argument_list.data[i]);
        }
        printf("[%d][ ", (int)expression->call.argument_list.len);
        print_expression(expression->call.function);
        printf("] ");
        break;
    case ExpressionKindBranch:
        printf("[ ");
        print_expression(expression->branch.condition);
        printf("] ");
        printf("[ ");
        print_expression(expression->branch.if_branch);
        printf("] ");
        printf("[ ");
        print_expression(expression->branch.else_branch);
        printf("] ");
        printf("? ");
        break;
    case ExpressionKindWhile:
        printf("[ ");
        print_expression(expression->while_loop.condition);
        printf("] ");
        printf("[ ");
        print_expression(expression->while_loop.body);
        printf("] ");
        printf("while ");
        break;
    case ExpressionKindLet:
        printf("let %.*s = ",
            (int)expression->let.identifier.len,
            expression->let.identifier.data);
        print_expression(expression->let.value);
        printf("in ");
        print_expression(expression->let.body);
        break;
    case ExpressionKindLambda:
        printf("[ %.*s => ",
            (int)expression->lambda.identifier.len,
            expression->lambda.identifier.data);
        print_expression(expression->lambda.body);
        printf("] ");
        break;
    }
}

// TODO: some errors should totally return NULL instead of exiting so that we have
// a stack of error messages

//  a null pointer is used to indicate an empty expression
struct Expression* expr_bp(struct Lexer* lexer, unsigned int minbp)
{
    // NOTE: the rule for expressions is to allocate them and then
    // never discard them, we need to keep them around the whole time
    // as long we never create more expressions than necessary we don't
    // leak memory

    struct Expression* lhs;

    struct Token t = next_token(lexer);
    // TODO: make sure we exhaustively handle this it's very important

    if (t.kind == TokenKindIdent) {
        // identifier
        struct Token t2 = peek_token(lexer);

        if (seq(t2.lexeme, "=>")) {
            next_token(lexer);
            int rbp;
            prefix_binding_power(make_string("lambda"), &rbp);
            struct Expression* rhs = expr_bp(lexer, rbp);
            if (rhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression following the lambda '%.*s =>' "
                    " after %d:%d in %s \n",
                    (int)t.lexeme.len,
                    t.lexeme.data,
                    t2.range.end.line,
                    t2.range.end.column,
                    t2.range.filename);
                exit(1);
            }
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindLambda;
            lhs->lambda.identifier = t.lexeme;
            lhs->lambda.body = rhs;
        } else {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindAtom;
            lhs->atom = t.lexeme;
        }
    } else if (t.kind == TokenKindNat) {
        // natural number
        lhs = malloc(sizeof(struct Expression));
        lhs->kind = ExpressionKindAtom;
        lhs->atom = t.lexeme;
    } else if (t.kind == TokenKindKeyword) { // hmmmm
        if (seq(t.lexeme, "break")) {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindBreak;
        } else if (seq(t.lexeme, "if")) { // if expression
            // TODO: decide the precedence of this + errors
            int rbp;
            prefix_binding_power(t.lexeme, &rbp);

            struct Expression* condition = expr_bp(lexer, rbp);
            if (condition == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the 'if' keyword "
                    " at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }
            struct Token t2;
            if (!seq((t2 = next_token(lexer)).lexeme, "then")) {
                fprintf(stderr,
                    "[Parsing Error] expected 'then' to continue 'if'"
                    " expression beginning at %d:%d instead found '%.*s' at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.filename);
                exit(1);
            }
            lhs = expr_bp(lexer, rbp);
            if (lhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the 'then' keyword"
                    " at %d:%d to continue the 'if' expression starting at %d:%d in %s \n",
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }
            // peek the next token to see if it an else
            struct Token t3 = peek_token(lexer);
            if (!seq(t3.lexeme, "else")) {
                struct Expression* new = malloc(sizeof(struct Expression));
                new->kind = ExpressionKindBranch;
                new->branch.condition = condition;
                new->branch.if_branch = lhs;
                new->branch.else_branch = NULL;
                lhs = new;
            } else {
                next_token(lexer);
                struct Expression* rhs = expr_bp(lexer, rbp);
                if (rhs == NULL) {
                    fprintf(stderr,
                        "[Parsing Error] missing expression after the 'else' keyword"
                        " at %d:%d to continue the 'if-else' expression starting at %d:%d in %s \n",
                        t3.range.start.line,
                        t3.range.start.column,
                        t.range.start.line,
                        t.range.start.column,
                        t.range.filename);
                    exit(1);
                }

                struct Expression* new = malloc(sizeof(struct Expression));
                new->kind = ExpressionKindBranch;
                new->branch.condition = condition;
                new->branch.if_branch = lhs;
                new->branch.else_branch = rhs;
                lhs = new;
            }

        } else if (seq(t.lexeme, "while")) { // while expression (this is more statement like)
            int rbp;
            prefix_binding_power(t.lexeme, &rbp);
            struct Expression* condition = expr_bp(lexer, rbp);
            if (condition == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the 'while' keyword"
                    " at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }
            struct Token t2;
            if (!seq((t2 = next_token(lexer)).lexeme, "do")) {
                fprintf(stderr,
                    "[Parsing Error] expected 'do' to continue 'while'"
                    " expression beginning at %d:%d instead found '%.*s' at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.filename);
                exit(1);
            }
            lhs = expr_bp(lexer, 0);
            if (lhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the 'do' keyword"
                    " at %d:%d to continue the 'while' expression starting at %d:%d in %s \n",
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }
            // peek the next token to see if it is done
            struct Token t3 = next_token(lexer);
            if (!seq(t3.lexeme, "done")) {
                fprintf(stderr,
                    "[Parsing Error] expected matching 'done' at %d:%d after the 'do' keyword"
                    " at %d:%d to end the 'while' expression starting at %d:%d instead got '%.*s' in %s \n",
                    t3.range.start.line,
                    t3.range.start.column,
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.start.line,
                    t.range.start.column,
                    (int)t3.lexeme.len,
                    t3.lexeme.data,
                    t.range.filename);
                exit(1);
            }
            struct Expression* new = malloc(sizeof(struct Expression));
            new->kind = ExpressionKindWhile;
            new->while_loop.condition = condition;
            new->while_loop.body = lhs;
            lhs = new;
        } else if (seq(t.lexeme, "let")) {
            int rbp;
            prefix_binding_power(t.lexeme, &rbp);
            struct Token t2 = next_token(lexer);
            if (t2.kind != TokenKindIdent) {
                fprintf(stderr,
                    "[Parsing Error] expected an identifier "
                    " to follow 'let' at %d:%d for a let expression"
                    " instead found '%.*s' at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.filename);
                exit(1);
            }
            struct Token t3 = next_token(lexer);
            if (!seq(t3.lexeme, "=")) {
                fprintf(stderr,
                    "[Parsing Error] expected a definition with '=' "
                    " to follow 'let %.*s' at %d:%d for a let expression"
                    " instead found '%.*s' at %d:%d in %s \n",
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t.range.start.line,
                    t.range.start.column,
                    (int)t3.lexeme.len,
                    t3.lexeme.data,
                    t2.range.start.line,
                    t2.range.start.column,
                    t.range.filename);
                exit(1);
            }
            struct Expression* rhs = expr_bp(lexer, rbp);
            if (rhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression on the right hand side of 'let' binding"
                    " 'let %.*s = ' at %d:%d in %s",
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }

            // TODO: i just realised i have information to literally put the piece of broken code
            struct Token t4 = next_token(lexer);
            if (!seq(t4.lexeme, ";")) {
                fprintf(stderr,
                    "[Parsing Error] expected ';' to follow the 'let' binding at %d:%d instead found '%.*s' at %d:%d in %s",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t4.lexeme.len,
                    t4.lexeme.data,
                    t4.range.start.line,
                    t4.range.start.column,
                    t.range.filename);
                exit(1);
            }
            struct Expression* body = expr_bp(lexer, rbp);
            if (body == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression on the right hand side of 'let' expression"
                    "after %d:%d in %s. \n\nA let binding must be of the form let <ident> = <expr1> ; <expr2>"
                    " where the value of the bound identifier is used in the body of the second expression",
                    t4.range.end.line,
                    t4.range.end.column,
                    t4.range.filename);
                exit(1);
            }
            struct Expression* new = malloc(sizeof(struct Expression));
            new->kind = ExpressionKindLet;
            new->let.identifier = t2.lexeme;
            new->let.body = body;
            new->let.value = rhs;

            lhs = new;
        } else {
            fprintf(stderr,
                "[Parsing Error] an expression was expected at %d:%d in %s and the keyword"
                " '%.*s' cannot be used to begin an expression. \n",
                t.range.start.line,
                t.range.start.column,
                t.range.filename,
                (int)t.lexeme.len,
                t.lexeme.data);
            return NULL;
        }
    } else if (t.kind == TokenKindLParen) {
        // parenthesised expressions
        struct Token t2;
        lhs = expr_bp(lexer, 0);
        if (lhs == NULL) {
            fprintf(stderr,
                "[Parsing Error] missing expression after the open parenthesis '('"
                " at %d:%d in %s \n",
                t.range.start.line,
                t.range.start.column,
                t.range.filename);
            exit(1);
        }
        if ((t2 = next_token(lexer)).kind != TokenKindRParen) {
            fprintf(stderr,
                "[Parsing Error] expected matching closing parenthesis"
                " ')' for '(' at %d:%d to enclose expression"
                " instead found '%.*s' at %d:%d in %s \n",
                t.range.start.line,
                t.range.start.column,
                (int)t2.lexeme.len,
                t2.lexeme.data,
                t2.range.start.line,
                t2.range.start.column,
                t.range.filename);
            exit(1);
        }
    } else if (t.kind == TokenKindOperator) {
        // prefix operators
        int rbp;
        if (prefix_binding_power(t.lexeme, &rbp)) {
            fprintf(stderr,
                "[Parsing Error] nonexistent prefix operator '%.*s' at %d:%d in %s \n",
                (int)t.lexeme.len,
                t.lexeme.data,
                t.range.start.line,
                t.range.start.column,
                t.range.filename);
            exit(1);
        }
        struct Expression* rhs = expr_bp(lexer, rbp);
        if (rhs == NULL) {
            fprintf(stderr,
                "[Parsing Error] missing expression on the right hand side"
                " of the prefix operator '%.*s' after %d:%d in %s \n",
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
    } else {
        // failure
        return NULL;
    }

    // extend the expression as much as possible using post and infix operators
    while (true) {
        struct Token t = peek_token(lexer);
        int lbp, rbp;

        // special post fixes
        if (t.kind == TokenKindLBracket) {
            // array access
            // TODO: currently useless because
            postfix_binding_power(t.lexeme, &lbp);
            if (lbp < minbp) {
                break;
            }

            struct Token t2;
            next_token(lexer);
            struct Expression* rhs = expr_bp(lexer, 0);
            // rbp = 0

            if (rhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression after the open bracket '[' "
                    " at %d:%d in %s \n",
                    t.range.end.line,
                    t.range.end.column,
                    t.range.filename);
                exit(1);
            }
            if ((t2 = next_token(lexer)).kind != TokenKindRBracket) {
                fprintf(stderr,
                    "[Parsing Error] expected matching closing bracket ']'"
                    " for '[' at %d:%d for indexed access,"
                    " instead found '%.*s' at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    (int)t2.lexeme.len,
                    t2.lexeme.data,
                    t2.range.start.line,
                    t2.range.start.column,
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
            // TODO: ERROR MESSAGES PLEASE

            // NOTE: disallow doing f(a, b, c,) because surely
            // function interfaces rarely change

            // TODO: this vode is repeated for all branches
            // but the error handling is different in these cases?? maybe
            // ahhhh this code is a disaster

            postfix_binding_power(t.lexeme, &lbp);
            if (lbp < minbp) {
                break;
            }

            // build a argument list
            struct Token t2;
            next_token(lexer);

            struct ArrayList(Expression) el = empty_list(Expression);

            // check if the argumetn list is completely empty
            if (peek_token(lexer).kind == TokenKindRParen) {
                next_token(lexer);
                goto make_call_expression;
            }

            // now assume that the argument list must have a bunch of expressions
            struct Expression* ilhs = expr_bp(lexer, 0);
            append(Expression, el, ilhs);

            if (ilhs == NULL) {
                fprintf(stderr,
                    "[Parsing Error] missing expression on the right hand side"
                    " of open parenthesis '(' inside the argument list beginning at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                exit(1);
            }

            while (true) {
                t2 = next_token(lexer);
                if (t2.kind == TokenKindRParen)
                    break;
                if (t2.kind != TokenKindComma) {
                    fprintf(stderr,
                        "[Parsing Error] expected comma ',' or closing parenthesis"
                        " starting at %d:%d for argument list"
                        " instead found '%.*s' at %d:%d in %s \n",
                        t.range.start.line,
                        t.range.start.column,
                        (int)t2.lexeme.len,
                        t2.lexeme.data,
                        t2.range.start.line,
                        t2.range.start.column,
                        t.range.filename);
                    exit(1);
                }
                ilhs = expr_bp(lexer, 0);
                if (ilhs == NULL) {
                    fprintf(stderr,
                        "[Parsing Error] missing expression on the right hand side"
                        " of comma ',' after %d:%d inside the argument list beginning at %d:%d in %s \n",
                        t2.range.end.line,
                        t2.range.end.column,
                        t.range.start.line,
                        t.range.start.column,
                        t.range.filename);
                    exit(1);
                }
                append(Expression, el, ilhs);
            }

        make_call_expression:

            struct Expression* new = malloc(sizeof(struct Expression));

            // horrible hacky code
            // TODO: ahhhhhh

            new->kind = ExpressionKindCall;
            new->call.function = lhs;
            new->call.argument_list = el;

            lhs = new;
            continue;
        }

        if (t.kind != TokenKindOperator && t.kind != TokenKindSemicolon)
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
                "[Parsing Error] nonexistent infix operator '%.*s' at %d:%d in %s \n",
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
                " of the infix operator '%.*s' after %d:%d in %s \n",
                (int)t.lexeme.len,
                t.lexeme.data,
                t.range.end.line,
                t.range.end.column,
                t.range.filename);
            exit(1);
        }

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
// end of expression parsing code
int main()
{
    DEFINE_ARRAYLIST(char);
    FILE* file = fopen("file.txt", "r"); // NOTE: just ignore the error for now

    int c;
    struct ArrayList(char) s = empty_list(char);
    while ((c = fgetc(file)) != EOF) {
        append(char, s, c);
    }
    append(char, s, '\0');
    fclose(file);

    const char* input = s.data;
    puts("Source:\n```");
    puts(input);

    puts("```\n\nLexical Analysis:");
    struct Lexer* lexer = make_lexer(input);
    struct Token t;
    while ((t = next_token(lexer)).kind != TokenKindEof) {
        print_token(t);
    }

    puts("\nParsing:");
    struct Expression* exp = expr(input);
    print_expression(exp);
}
