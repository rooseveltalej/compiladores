// ==============================
//         LEXER RULES
// ==============================
// MiniCSharpLexer.g4
// MiniCSharpLexer.g4
lexer grammar MiniCSharpLexer;

options { language = CSharp; }

// --- Keywords ---
CLASS: 'class';
VOID: 'void';
IF: 'if';
ELSE: 'else';
FOR: 'for';
WHILE: 'while';
BREAK: 'break';
RETURN: 'return';
READ: 'read';
WRITE: 'write';
NEW: 'new';
TRUE: 'true';
FALSE: 'false';
SWITCH: 'switch'; // <<< NUEVO
CASE: 'case';     // <<< NUEVO
DEFAULT: 'default'; // <<< NUEVO
USING: 'using';


// --- Operators & Punctuation ---
ASSIGN: '=';
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
EQUAL: '==';
NOTEQUAL: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';
AND: '&&';
OR: '||';
INC: '++';
DEC: '--';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
LBRACK: '[';
RBRACK: ']';
SEMI: ';';
COMMA: ',';
DOT: '.';
COLON: ':'; 

// --- Literales ---
INTCONST: '0' | [1-9] DIGIT*;
DOUBLECONST: DIGIT+ '.' DIGIT+;
CHARCONST: '\'' (ESC_SEQ | ~['\\]) '\'';
STRINGCONST: '"' (ESC_SEQ | ~["\\])* '"';

// --- Identifier ---
IDENT: LETTER (LETTER | DIGIT)*;

// --- Whitespace & Comments ---
WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' -> pushMode(CM_MODE), skip;

// --- Fragments ---
fragment LETTER: [a-zA-Z_];
fragment DIGIT: [0-9];
fragment ESC_SEQ: '\\' [btnr"'\\];

// --- Comment Mode (Para Anidamiento) ---
mode CM_MODE;
    CM_START: '/*' -> pushMode(CM_MODE), skip;
    CM_END: '*/' -> popMode, skip;
    CM_TEXT: . -> skip;