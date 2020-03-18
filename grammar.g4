/* A very simplified version of C grammar, given in ANTLR form, that is used as reference by the parser. */

grammar minic;

translationUnit
  : (decl | function)*
  ;

function
  : type ID '(' params* ')' block ;

params
  : param ( ',' param )*
  ;

param
  : type ID
  ;

block
  : '{' (decl | stmt)* '}'
  ;

stmt
  : block
  | expr? ';'
  | 'if' expr 'then' stmt 'else' stmt
  | 'while' expr stmt
  ;

expr
  : ID
  | CONST
  | '(' expr ')'
  | expr ('/' | '*') expr
  | expr ('+' | '-') expr
  | expr ('>' | '<' | '==') expr
  | ID '=' expr
  ;

decl
  : type ID
  ;

type
  : 'int'
  | 'void'
  ;

ID : [a-zA-Z_][a-zA-Z_0-9]* ;

CONST : [0-9]+ ;

WS : [ \t\n\r]+ -> skip ;
BlockComment : '*/' .*? '/*' -> skip ;
LineComment  : '//' ~[\r\n]* -> skip ;

