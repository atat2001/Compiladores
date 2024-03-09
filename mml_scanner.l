%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}
%x X_COMMENT
%x X_STRING X_BACKSLASH 
%%
  yydebug=1; set_debug(1);

"//".*                 ; /* ignore comments */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|"\n"      ;  /* ignore inside comments */

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 

"&&"                   return tAND;
"||"                   return tOR;

"while"                return tWHILE; 
"if"                   return tIF;
"elif"                 return tELIF;
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;
"else"                 return tELSE;
"!"                    return tPRINT;
"!!"                   return tPRINTLN;
"begin"                return tBEGIN; 
"end"                  return tEND;

"auto"                   return tTYPE_AUTO;
"int"                    return tTYPE_INT;
"double"                 return tTYPE_DOUBLE;
"string"                 return tTYPE_STRING;
"void"                   return tTYPE_VOID;

"foreign"                return tFOREIGN;
"forward"                return tFORWARD;
"public"                 return tPUBLIC;

"null"                   return tNULL;
"input"                  return tINPUT;
"sizeof"                 return tSIZEOF;
"->"                     return tATR;


([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { yylval.d = strtod(yytext, NULL); return tDOUBLE; }

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

"\""                           yy_push_state(X_STRING);
<X_STRING>\\                   yy_push_state(X_BACKSLASH);
<X_STRING>"\""                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_STRING>.                    strlit << *yytext;

<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();
<X_BACKSLASH>"\""              strlit << '\"'; yy_pop_state();

<X_BACKSLASH>[0-7]{1,3} {
                                 strlit << (char)(unsigned char)strtoul(yytext, NULL, 8);
                                 if(errno==ERANGE) yyerror("Overflow...");
                                 yy_pop_state();
                               }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state();

0|[1-9][0-9]*                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;
0[0-7]+             yylval.i = strtol(yytext, nullptr, 8); return tINTEGER;
0[0-9]+             yyerror("Invalid octal number");

[-()<>=+*/%;{}.\[\]?@~,]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                       puts(yytext); yyerror("Unknown character");

%%
