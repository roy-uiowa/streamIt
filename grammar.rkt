#lang brag

program        : declaration*
;Declarations
declaration    : classDecl
               | funDecl
               | varDecl
               | statement
               | scheduler
               | streamObject
               | comments
/comments       : "//" *
scheduler      : /type /IDENTIFIER /"{" /"initSched" initSched /"steadySched" steadySched /"}"
initSched      : block
steadySched    : block

streamObject   : channel_type (/"stateful")* obj_type function
channel_type   : type /"->" type
type           : INT
               | FLOAT
               |"void"
obj_type       : FILTER | SPLITJOIN | PIPELINE

classDecl      : "class" IDENTIFIER ( "<" IDENTIFIER )?
                 "{" function* "}" ;
funDecl        : /"fun" function ;
varDecl        : "var" IDENTIFIER ( /"=" expression )? /";" ;
               | type IDENTIFIER ( /"=" expression )? /";"
               | type IDENTIFIER /'[' expression /']' /";"
               | type '[' expression ']' IDENTIFIER /";"

;Statements
statement      : exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | addStatement
               | initBlock
               | workBlock
               | block ;
               | splitjoinStmt

exprStmt       : expression /";" ;
forStmt        : /"for" /"(" ( varDecl | exprStmt | ";" )
                           expression? /";"
                           expression? /")" statement ;
ifStmt         : /"if" /"(" expression /")" statement ( /"else" statement )? ;
printStmt      : "print" expression /";" ;
returnStmt     : /"return" expression? ";" ;
whileStmt      : /"while" "(" expression ")" statement
addStatement   : /"add" call /";"
initBlock      : /"init" block
workBlock      : /"work" rateDecl? block
block          : /"{" declaration* /"}" ;
splitjoinStmt  : ("split" | "join") splitjoinType (/"(" arguments? /")")* /";"
splitjoinType  : "duplicate" | "roundrobin"
;Expressionsa
rateDecl       : primary*
expression     : assignment

assignment     : ( call "." )? IDENTIFIER (/"[" assignment /"]")* /"=" assignment
               |logic_or

logic_or       : logic_and ( "or" logic_and )* ;
logic_and      : equality ( "and" equality )* ;
equality       : comparison ( ( "!=" | "==" ) comparison )* ;
comparison     : addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       : multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication : unary ( ( "/" | "*" ) unary )* ;
unary          : ( "!" | "-" | "++" | "--" ) unary
               | unary ("++" | "--")
               | call ;


call           : primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary        : "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;

;Utility Rules
function       : IDENTIFIER /"(" parameters? /")" block ;
parameters     : args (/"," args)*
args           : type IDENTIFIER ;
arguments      : expression ( /"," expression )* ;


;Lexical Grammar
;NUMBER         : DIGIT+ ( "." DIGIT+ )? ;
;STRING         : '"' <any char except '"'>* '"' ;
;IDENTIFIER     : ALPHA ( ALPHA | DIGIT )* ;
;ALPHA          : 'a' ... 'z' | 'A' ... 'Z' | '_' ;
;DIGIT          : '0' ... '9' ;