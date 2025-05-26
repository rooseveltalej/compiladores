// ==============================
//         PARSER RULES
// ==============================

parser grammar MiniCSharpParser;


options {
    tokenVocab = MiniCSharpLexer;
    language = CSharp;
}

// --- Start Rule ---
program: CLASS IDENT LBRACE (varDecl | classDecl | methodDecl)* RBRACE;

// --- Declarations ---
varDecl: type IDENT (COMMA IDENT)* SEMI;

classDecl: CLASS IDENT LBRACE varDecl* RBRACE;

methodDecl: (type | VOID) IDENT LPAREN formPars? RPAREN block;

formPars: type IDENT (COMMA type IDENT)*;

type: IDENT (LBRACK RBRACK)?;

// --- Statements ---
statement:
    designator (ASSIGN expr | LPAREN actPars? RPAREN | INC | DEC) SEMI #DesignatorStatement
    | IF LPAREN condition RPAREN statement (ELSE statement)? #IfStatement
    | FOR LPAREN expr? SEMI condition? SEMI statement? RPAREN statement #ForStatement
    | WHILE LPAREN condition RPAREN statement #WhileStatement
    | BREAK SEMI #BreakStatement
    | RETURN expr? SEMI #ReturnStatement
    | READ LPAREN designator RPAREN SEMI #ReadStatement
    | WRITE LPAREN expr (COMMA number)? RPAREN SEMI #WriteStatement
    | block #BlockStatement
    | SEMI #EmptyStatement
    ;

block: LBRACE (varDecl | statement)* RBRACE;

actPars: expr (COMMA expr)*;

// --- Conditions ---
condition: condTerm (OR condTerm)*;

condTerm: condFact (AND condFact)*;

condFact: expr relop expr;

// --- Expressions ---
cast: LPAREN type RPAREN;

expr: MINUS? cast? term (addop term)*;

term: factor (mulop factor)*;

factor:
    designator (LPAREN actPars? RPAREN)? #DesignatorFactor
    | number #NumberFactor
    | CHARCONST #CharFactor
    | STRINGCONST #StringFactor
    | TRUE #BoolFactor
    | FALSE #BoolFactor
    | NEW IDENT (LPAREN RPAREN | LBRACK expr RBRACK)? #NewFactor // Ajustado para new Class() y new type[expr]
    | LPAREN expr RPAREN #ParenFactor
    ;

designator: IDENT (DOT IDENT | LBRACK expr RBRACK)*;

// --- Helper Rules ---
number: INTCONST | DOUBLECONST; // Unifica los literales numéricos

relop: EQUAL | NOTEQUAL | GT | GE | LT | LE;
addop: PLUS | MINUS;
mulop: MULT | DIV | MOD;