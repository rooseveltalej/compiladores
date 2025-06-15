// ==============================
//         PARSER RULES
// ==============================

// MiniCSharpParser.g4
parser grammar MiniCSharpParser;

options {
    tokenVocab = MiniCSharpLexer;
    language = CSharp;
}

// --- Start Rule ---
// Un programa ahora puede comenzar con cero o más directivas 'using',
// seguidas por la declaración de la clase principal.
program: usingDirective* CLASS IDENT LBRACE (varDecl | classDecl | methodDecl)* RBRACE;

// --- Using Directive ---
// Define la estructura de una directiva 'using'.
// Ej: using System; o using MiModulo.Utils;
usingDirective: USING qualifiedIdent SEMI;

// Un identificador calificado puede ser una secuencia de IDENTs separados por DOT.
// Ej: System, System.Collections, MiNamespace.MiClaseInterna
qualifiedIdent: IDENT (DOT IDENT)*;

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
    | FOR LPAREN expr SEMI condition? SEMI statement? RPAREN statement #ForStatement
    | WHILE LPAREN condition RPAREN statement #WhileStatement
    | SWITCH LPAREN expr RPAREN LBRACE switchCase* defaultCase? RBRACE #SwitchStatement 
    | BREAK SEMI #BreakStatement
    | RETURN expr? SEMI #ReturnStatement
    | READ LPAREN designator RPAREN SEMI #ReadStatement
    | WRITE LPAREN expr (COMMA number)? RPAREN SEMI #WriteStatement
    | block #BlockStatement
    | SEMI #EmptyStatement
    ;


switchCase:
    CASE constant COLON statement* // Permitimos múltiples statements por case
    ;

defaultCase:
    DEFAULT COLON statement* // Permitimos múltiples statements para default
    ;


constant:
    number
    | CHARCONST
    
   
    ;



block: LBRACE (varDecl | statement)* RBRACE;
actPars: expr (COMMA expr)*;
condition: condTerm (OR condTerm)*;
condTerm: condFact (AND condFact)*;
condFact: expr (relop expr)?;
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
    | NEW IDENT (LPAREN RPAREN | LBRACK expr RBRACK)? #NewFactor
    | LPAREN expr RPAREN #ParenFactor
    ;
designator: IDENT (DOT IDENT | LBRACK expr RBRACK)*;
number: INTCONST | DOUBLECONST;
relop: EQUAL | NOTEQUAL | GT | GE | LT | LE;
addop: PLUS | MINUS;
mulop: MULT | DIV | MOD;