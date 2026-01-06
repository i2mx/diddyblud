// TODO: YOU NEED NEED NEED NEED TO REFACTOR THIS

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
    return strncmp(s1.data, s2, s1.len) == 0
        && s1.len == strlen(s2);
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

    // I asked chatgpt to write this function and it's a little embarrassing.
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
    // so this should never leak memory

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
    }
    else {
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

// determines if a character is a "delimiter"
static inline bool is_delim(const char c)
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

// returns the exact delimiter type of a character
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

    if (c == '#') {
        while (peek(lexer) != '\n' && peek(lexer) != '\0') {
            advance(lexer);
        }
        return next_token(lexer);
    }

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

    if (is_delim(c)) {
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
                || seq(s, "continue")
                || seq(s, "false")
                || seq(s, "true")
                || seq(s, "void")
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
    ExpressionKindContinue,
    ExpressionKindTrue,
    ExpressionKindFalse,
    ExpressionKindVoid,
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
    if (seq(op, "if") || seq(op, "while")
        || seq(op, "let") || seq(op, "lambda")) {
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
        }
        else {
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
    case ExpressionKindBreak:
        printf("break ");
        break;
    case ExpressionKindContinue:
        printf("continue ");
        break;
    case ExpressionKindTrue:
        printf("true ");
        break;
    case ExpressionKindFalse:
        printf("false ");
        break;
    case ExpressionKindVoid:
        printf("void ");
        break;
    case ExpressionKindLet:
        printf("( let %.*s = ",
            (int)expression->let.identifier.len,
            expression->let.identifier.data);
        print_expression(expression->let.value);
        printf("in ");
        print_expression(expression->let.body);
        printf(") ");
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
struct Expression* expr_bp(struct Lexer* lexer, unsigned int minBp, bool enableBreak)
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
            struct Expression* rhs = expr_bp(lexer, rbp, false);
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
        }
        else {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindAtom;
            lhs->atom = t.lexeme;
        }
    }
    else if (t.kind == TokenKindNat) {
        // natural number
        lhs = malloc(sizeof(struct Expression));
        lhs->kind = ExpressionKindAtom;
        lhs->atom = t.lexeme;
    }
    else if (t.kind == TokenKindKeyword) { // hmmm
        if (seq(t.lexeme, "break")) {
            if (!enableBreak) {
                fprintf(stderr,
                    "[Parsing Error] illegal break expression found"
                    " at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                return NULL;
            }
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindBreak;
            // break out early
            return lhs;
        }
        else if (seq(t.lexeme, "continue")) {
            if (!enableBreak) {
                fprintf(stderr,
                    "[Parsing Error] illegal continue expression found"
                    " at %d:%d in %s \n",
                    t.range.start.line,
                    t.range.start.column,
                    t.range.filename);
                return NULL;
            }
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindContinue;
            return lhs;
        }
        else if (seq(t.lexeme, "true")) {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindTrue;
        }
        else if (seq(t.lexeme, "false")) {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindFalse;
        }
        else if (seq(t.lexeme, "void")) {
            lhs = malloc(sizeof(struct Expression));
            lhs->kind = ExpressionKindVoid;
            return lhs;
        }
        else if (seq(t.lexeme, "if")) { // if expression
            // TODO: decide the precedence of this + errors
            int rbp;
            prefix_binding_power(t.lexeme, &rbp);

            struct Expression* condition = expr_bp(lexer, rbp, false);
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
            lhs = expr_bp(lexer, rbp, enableBreak);
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
            }
            else {
                next_token(lexer);
                struct Expression* rhs = expr_bp(lexer, rbp, enableBreak);
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

        }
        else if (seq(t.lexeme, "while")) { // while expression (this is more statement like)
            int rbp;
            prefix_binding_power(t.lexeme, &rbp);
            struct Expression* condition = expr_bp(lexer, rbp, false);
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
            // ENABLE BREAKS
            lhs = expr_bp(lexer, 0, true);
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
        }
        else if (seq(t.lexeme, "let")) {
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
            struct Expression* rhs = expr_bp(lexer, rbp, false);
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
            // in order to correctly parse let x = 1 in x ; 1 as let x = 1 ; (x ; 1)
            struct Expression* body = expr_bp(lexer, 0, enableBreak);
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
        }
        else {
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
    }
    else if (t.kind == TokenKindLParen) {
        // parenthesised expressions
        struct Token t2;
        lhs = expr_bp(lexer, 0, enableBreak);
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
    }
    else if (t.kind == TokenKindOperator) {
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
        struct Expression* rhs = expr_bp(lexer, rbp, false);
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
    }
    else {
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
            if (lbp < minBp) {
                break;
            }

            struct Token t2;
            next_token(lexer);
            struct Expression* rhs = expr_bp(lexer, 0, false);
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

        }
        else if (t.kind == TokenKindLParen) {
            // TODO: ERROR MESSAGES PLEASE

            // NOTE: disallow doing f(a, b, c,) because surely
            // function interfaces rarely change

            // TODO: this code is repeated for all branches
            // but the error handling is different in these cases?? maybe
            // ahh this code is a disaster

            postfix_binding_power(t.lexeme, &lbp);
            if (lbp < minBp) {
                break;
            }

            // build a argument list
            struct Token t2;
            next_token(lexer);

            struct ArrayList(Expression) el = empty_list(Expression);

            // check if the argument list is completely empty
            if (peek_token(lexer).kind == TokenKindRParen) {
                next_token(lexer);
                goto make_call_expression;
            }

            // now assume that the argument list must have a bunch of expressions
            struct Expression* iLhs = expr_bp(lexer, 0, false);
            append(Expression, el, iLhs);

            if (iLhs == NULL) {
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
                iLhs = expr_bp(lexer, 0, false);
                if (iLhs == NULL) {
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
                append(Expression, el, iLhs);
            }

        make_call_expression:

            struct Expression* new = malloc(sizeof(struct Expression));

            // horrible hacky code
            // TODO: ahh

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
        // when using postfix operators, i don't really like this but
        // it'll have to do because chaining is quite nice i guess

        if (postfix_binding_power(t.lexeme, &lbp) == 0) {
            // postfix operators
            if (lbp < minBp)
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

        if (lbp < minBp)
            break;

        next_token(lexer);

        // once we have taken an operator we need an expression on the right hand side
        struct Expression* rhs = expr_bp(lexer, rbp, seq(t.lexeme, ";") ? enableBreak : false);
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
    return expr_bp(lexer, 0, false);
}
// end of expression parsing code

// TYPE CHECKING
enum TypeKind {
    TYPE_INT,
    TYPE_BOOL,
    TYPE_VOID,
    TYPE_VAR, // represents unknown type that we can work out later
    TYPE_FUN,
};

typedef struct Type Type;
struct Type {
    enum TypeKind kind;
    int id;
    struct Type* parent;
    union {
        struct {
            Type* from;
            Type* to;
        } fun;
    };
};

static int next_type_id = 0;
Type* new_type(enum TypeKind kind)
{
    Type* t = malloc(sizeof(Type));
    t->kind = kind;
    t->id = next_type_id++;
    t->parent = t;
    return t;
}

Type* type_var(void) { return new_type(TYPE_VAR); }
Type* type_int(void)
{
    static Type t = { TYPE_INT, -1, NULL };
    if (!t.parent)
        t.parent = &t;
    return &t;
}
Type* type_bool(void)
{
    static Type t = { TYPE_BOOL, -2, NULL };
    if (!t.parent)
        t.parent = &t;
    return &t;
}
Type* type_void(void)
{
    static Type t = { TYPE_VOID, -3, NULL };
    if (!t.parent)
        t.parent = &t;
    return &t;
}

Type* type_fun(Type* a, Type* b)
{
    Type* t = new_type(TYPE_FUN);
    t->fun.from = a;
    t->fun.to = b;
    return t;
}

// find the representative type of the union-find set
Type* find(Type* t)
{
    return t->parent == t
        ? t
        : (t->parent = find(t->parent));
}

// checks if type variable v occurs in type t in order
// to prevent circular / infinite types
bool occurs(Type* v, Type* t)
{
    t = find(t);
    if (t == v)
        return true;
    if (t->kind == TYPE_FUN)
        return occurs(v, t->fun.from) || occurs(v, t->fun.to);
    return false;
}

// attempt to unify two types, returning true on success
// that is we create the constraint that a == b
bool unify(Type* a, Type* b)
{
    // printf("Unifying types %d and %d\n", a->id, b->id);
    a = find(a);
    b = find(b);

    if (a == b)
        return true;

    if (a->kind == TYPE_VAR) {
        if (occurs(a, b))
            return false;
        a->parent = find(b);
        return true;
    }

    if (b->kind == TYPE_VAR) {
        if (occurs(b, a))
            return false;
        b->parent = find(a);
        return true;
    }

    if (a->kind != b->kind)
        return false;

    if (a->kind == TYPE_FUN) {
        return unify(a->fun.from, b->fun.from)
            && unify(a->fun.to, b->fun.to);
    }

    return true;
}

// represents a generalized polymorphic type such as forall a. a -> a
DEFINE_ARRAYLIST(int);
struct TypeScheme {
    Type* type;
    struct ArrayList(int) vars; // quantified variable ids
};

// assigns a type scheme to a variable name for example id : forall a. a -> a
// and x : int
struct TypeBinding {
    struct String name;
    struct TypeScheme scheme;
};
typedef struct TypeBinding TypeBinding;
DEFINE_ARRAYLIST(TypeBinding);

// a type environment simply maps variable names to type schemes, it is a collection of
// type bindings
struct TypeEnv {
    struct ArrayList(TypeBinding) bindings;
};

// writes into seen all the free type variables in type t, that is those not bound
// by a type scheme
void collect_free(Type* t, bool* seen)
{
    t = find(t);
    if (t->kind == TYPE_VAR) {
        seen[t->id] = true;
    }
    else if (t->kind == TYPE_FUN) {
        collect_free(t->fun.from, seen);
        collect_free(t->fun.to, seen);
    }
}
// does the same thing as collect except that it unwrites from seen in order to remove
// type variables that are bound in the environment.
void uncollect_free(Type* t, bool* seen)
{
    t = find(t);
    if (t->kind == TYPE_VAR) {
        seen[t->id] = false;
    }
    else if (t->kind == TYPE_FUN) {
        uncollect_free(t->fun.from, seen);
        uncollect_free(t->fun.to, seen);
    }
}

// the point is that in a let binding the type scheme assigned to (for example)
// id in `let id = x => x in ...` is generalized to forall a. a -> a or literally
// TypeScheme { type: a -> a, vars: [a] } where a is a type variable.
// NOTE: this is very tricky to get right
struct TypeScheme generalize(Type* t, struct TypeEnv* env)
{
    // we quantify with all the free type variables in t 
    // that are not in the environment
    bool* seen = calloc(next_type_id, sizeof(bool));
    collect_free(t, seen);

    struct ArrayList(int) vars = empty_list(int);
    for (int i = 0; i < next_type_id; i++)
        if (seen[i])
            append(int, vars, i);

    free(seen);
    return (struct TypeScheme) { t, vars };
}

Type* _clone(Type* t, Type** map, bool* is_qualified)
{
    t = find(t);
    if (t->kind == TYPE_VAR && !is_qualified[t->id])
        // we skip instantiating a type variable only if it's a variable and qualified
        return t;

    if (t->kind == TYPE_VAR) {
        if (map[t->id] == NULL)
            map[t->id] = type_var();
        return map[t->id];
    }
    if (t->kind == TYPE_FUN)
        return type_fun(
            _clone(t->fun.from, map, is_qualified),
            _clone(t->fun.to, map, is_qualified));
    return t;
}

// replaces quantified type variables in s with 'fresh' type variables meaning that
// s is sort of a template and here we use the template and create a new instance of it
Type* instantiate(struct TypeScheme* s)
{
    Type** map = calloc(next_type_id, sizeof(Type*));
    bool* is_qualified = calloc(next_type_id, sizeof(bool));

    // if it's not qualified there is no need in re-instantiating it
    for (size_t i = 0; i < s->vars.len; i++) {
        is_qualified[s->vars.data[i]] = true;
    }
    // this is needed because in a0 -> a0 we need to make sure both a0s get instantiated
    // as the same type variable.

    Type* result = _clone(s->type, map, is_qualified);
    free(map);
    free(is_qualified);
    return result;
}

// finds an identifier in name in the environment and then instantiates its type scheme
Type* lookup_env(struct TypeEnv* env, struct String name)
{
    for (size_t i = 0; i < env->bindings.len; ++i) {
        TypeBinding b = env->bindings.data[i];
        if (b.name.len == name.len
            && strncmp(
                b.name.data,
                name.data,
                name.len)
            == 0) {
            return instantiate(&b.scheme);
        }
    }
    fprintf(stderr, "[Type Error] unbound variable '%.*s'\n",
        (int)name.len, name.data);
    exit(1);
}

struct TypeEnv* clone_env(struct TypeEnv* env)
{
    struct TypeEnv* new_env = malloc(sizeof(struct TypeEnv));
    new_env->bindings = empty_list(TypeBinding);
    for (size_t i = 0; i < env->bindings.len; i++) {
        append(TypeBinding, new_env->bindings, env->bindings.data[i]);
    }
    return new_env;
}

void print_type(Type* t)
{
    t = find(t);
    switch (t->kind) {
    case TYPE_INT:
        printf("Int");
        break;
    case TYPE_BOOL:
        printf("Bool");
        break;
    case TYPE_VOID:
        printf("Void");
        break;
    case TYPE_VAR:
        printf("t%d", t->id);
        // print the parent
        // printf("[-> t%d]", find(t)->id);
        break;
    case TYPE_FUN:
        printf("(");
        print_type(t->fun.from);
        printf(" -> ");
        print_type(t->fun.to);
        printf(")");
        break;
    }
}

// pretty print env
void print_env(struct TypeEnv* env)
{
    puts("---\nType Environment:");
    for (size_t i = 0; i < env->bindings.len; i++) {
        TypeBinding b = env->bindings.data[i];
        printf("%.*s : ", (int)b.name.len, b.name.data);
        if (b.scheme.vars.len > 0) {
            printf("forall");
            for (size_t j = 0; j < b.scheme.vars.len; j++) {
                printf(" t%d", b.scheme.vars.data[j]);
            }
            printf(". ");
        }
        print_type(b.scheme.type);
        printf("\n");
    }
    puts("---\n");
}


// HM-type inference algorithm
Type* infer(struct Expression* e, struct TypeEnv* env)
{
    print_env(env);
    if (e == NULL) {
        printf("NULL EXPR\n");
        exit(1);
    }

    switch (e->kind) {
    case ExpressionKindAtom: {
        struct String s = e->atom;
        // numbers are int
        bool is_number = true;
        for (size_t i = 0; i < s.len; i++) {
            if (!isdigit(s.data[i])) {
                is_number = false;
                break;
            }
        }
        if (is_number)
            return type_int();
        // otherwise it's a variable
        return lookup_env(env, s);
    }
    case ExpressionKindMonad: {
        Type* rhs = infer(e->monad.expr, env);
        struct String op = e->monad.operation;
        if (seq(op, "-") || seq(op, "+")) {
            if (!unify(rhs, type_int())) {
                fprintf(stderr, "[Type Error] unary '%.*s' operator requires Int operand\n",
                    (int)op.len, op.data);
                exit(1);
            }
            return type_int();
        }
        if (seq(op, "!")) {
            if (!unify(rhs, type_bool())) {
                fprintf(stderr, "[Type Error] unary '%.*s' operator requires Bool operand\n",
                    (int)op.len, op.data);
                exit(1);
            }
            return type_bool();
        }
        fprintf(stderr, "[Type Error] unknown unary operator '%.*s'\n",
            (int)op.len, op.data);
        exit(1);
    }
    case ExpressionKindDyad: {
        Type* left = infer(e->dyad.left, env);
        Type* right = infer(e->dyad.right, env);
        struct String op = e->dyad.operation;

        if (seq(op, "+") || seq(op, "-") || seq(op, "*") || seq(op, "/") || seq(op, "%")) {
            if (!unify(left, type_int())) {
                fprintf(stderr, "[Type Error] binary '%.*s' operator requires Int left operand\n",
                    (int)op.len, op.data);
                exit(1);
            }

            if (!unify(right, type_int())) {
                fprintf(stderr, "[Type Error] binary '%.*s' operator requires Int right operand\n",
                    (int)op.len, op.data);
                exit(1);
            }

            return type_int();
        }
        if (seq(op, "==") || seq(op, "!=") || seq(op, "<") || seq(op, "<=") || seq(op, ">") || seq(op, ">=")) {
            if (!unify(left, right)) {
                fprintf(stderr, "[Type Error] binary '%.*s' operator requires both operands to be of the same type\n",
                    (int)op.len, op.data);
                exit(1);
            }
            return type_bool();
        }
        if (seq(op, "&&") || seq(op, "||")) {
            if (!unify(left, type_bool())) {
                fprintf(stderr, "[Type Error] binary '%.*s' operator requires Bool left operand\n",
                    (int)op.len, op.data);
                exit(1);
            }
            if (!unify(right, type_bool())) {
                fprintf(stderr, "[Type Error] binary '%.*s' operator requires Bool right operand\n",
                    (int)op.len, op.data);
                exit(1);
            }
            return type_bool();
        }
        if (seq(op, ";"))
            return right;

        fprintf(stderr, "[Type Error] unknown binary operator '%.*s'\n",
            (int)op.len, op.data);
        exit(1);
    }
    case ExpressionKindCall: {
        Type* fn = infer(e->call.function, env);
        for (size_t i = 0; i < e->call.argument_list.len; i++) {
            Type* arg = infer(e->call.argument_list.data[i], env);
            Type* ret = type_var();
            if (!unify(fn, type_fun(arg, ret))) {
                fprintf(stderr, "[Type Error] cannot apply non-function type\n");
                exit(1);
            }
            fn = ret;
        }
        return fn;
    }
    case ExpressionKindLet: {
        Type* val_type = infer(e->let.value, env);
        struct TypeScheme scheme = generalize(val_type, env);

        // TODO: do we use the same environment or do we clone it?
        struct TypeEnv* local_env = clone_env(env);
        TypeBinding binding = { e->let.identifier, scheme };
        append(TypeBinding, local_env->bindings, binding);
        // TODO: free here
        return infer(e->let.body, local_env);
    }
    case ExpressionKindLambda: {
        Type* param_type = type_var();

        // create a new local environment there is a better way to do this i think which is just to
        // append and then reset the size of the environment later TODO: what i just said.
        struct TypeEnv* local_env = clone_env(env);
        TypeBinding binding = { e->lambda.identifier, { param_type, empty_list(int) } };
        append(TypeBinding, local_env->bindings, binding);
        // TODO: free here    
        Type* body_type = infer(e->lambda.body, local_env);
        return type_fun(param_type, body_type);
    }
    case ExpressionKindBranch: { // if expressions
        Type* cond = infer(e->branch.condition, env);
        unify(cond, type_bool());

        Type* if_t = infer(e->branch.if_branch, env);
        if (e->branch.else_branch == NULL) {
            return type_void();
        }
        Type* else_t = infer(e->branch.else_branch, env);
        if (!unify(if_t, else_t)) {
            fprintf(stderr, "[Type Error] branches of 'if' expression must have the same type\n");
            exit(1);
        }
        return if_t;
    }
    case ExpressionKindWhile: {
        Type* cond = infer(e->while_loop.condition, env);
        unify(cond, type_bool());
        infer(e->while_loop.body, env);
        return type_void();
    }
    case ExpressionKindBreak:
    case ExpressionKindContinue:
        return type_void();
    case ExpressionKindTrue:
    case ExpressionKindFalse:
        return type_bool();
    case ExpressionKindVoid:
        return type_void();
    default:
        fprintf(stderr, "[Type Error] unknown expression kind\n");
        exit(1);
    }
}

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

    puts("\n\nParsing:");
    struct Expression* exp = expr(input);
    print_expression(exp);

    puts("\n\nType Inference:");
    struct TypeEnv env = { .bindings = empty_list(TypeBinding) };
    Type* ty = infer(exp, &env);
    printf("Inferred type: ");
    print_type(ty);
    printf("\n");
}
