// ==============================
//         LEXER RULES
// ==============================
lexer grammar MiniCSharpLexer;


// Palabras clave
CLASS       : 'class';
VOID        : 'void';
IF          : 'if';
ELSE        : 'else';
FOR         : 'for';
WHILE       : 'while';
BREAK       : 'break';
RETURN      : 'return';
READ        : 'read';
WRITE       : 'write';
NEW         : 'new';
TRUE        : 'true';
FALSE       : 'false';

// Operadores y símbolos
ASSIGN      : '=';
PLUS        : '+';
MINUS       : '-';
MULT        : '*';
DIV         : '/';
MOD         : '%';
EQUAL       : '==';
NOTEQUAL    : '!=';
LT          : '<';
LE          : '<=';
GT          : '>';
GE          : '>=';
AND         : '&&';
OR          : '||';
INC         : '++';
DEC         : '--';

LPAREN      : '(';
RPAREN      : ')';
LBRACE      : '{';
RBRACE      : '}';
LBRACK      : '[';
RBRACK      : ']';
SEMI        : ';';
COMMA       : ',';
DOT         : '.';

// Literales
fragment DIGIT : [0-9];
fragment LETTER : [a-zA-Z_];

INTCONST    : '0' | [1-9] DIGIT*;
DOUBLECONST : DIGIT+ '.' DIGIT+;

CHARCONST
    : '\'' ( ~['\\\r\n] | '\\' . ) '\'' ;

STRINGCONST
    : '"' ( ~["\\\r\n] | '\\' . )* '"' ;

// Identificadores
IDENT       : LETTER (LETTER | DIGIT)* ;

// Comentarios y espacios
WS
    : [ \t\r\n]+ -> skip
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

COMMENT
    : '/*' (COMMENT | .)*? '*/' -> skip
    ;
