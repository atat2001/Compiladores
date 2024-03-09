%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;    /* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  mml::block_node      *block;

  std::vector<std::shared_ptr<cdk::basic_type>>   *vector;

  mml::program_node * program;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tWHILE tIF tPRINT tPRINTLN tINPUT tBEGIN tEND tRETURN tSTOP tNEXT
%token tTYPE_STRING tTYPE_INT tTYPE_DOUBLE tTYPE_AUTO tTYPE_VOID tSIZEOF
%token tPUBLIC tFORWARD tFOREIGN tPRIVATE
%token tATR
%token tNULL

%nonassoc tIF
%nonassoc tELSE tELIF

%right '='
%left tAND tOR
%right '~'
%left tEQ tNE
%left '>' '<' tGE tLE
%left '+' '-'
%left '*' '/' '%'
%right tUMINUS


%type<sequence> decls_opt decls insts_opt insts exprs exprs_opt vars begin
%type<block> main block
%type<node> main_opt decl stop next return inst inst_conditional if_false inst_iteration var
%type<vector> types_opt types
%type<type> type function_type
%type<expression> expr integer double func_definition
%type<lvalue> lval
%type<s> string


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%


begin : decls_opt main_opt { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
      ;

main_opt : /*empty*/     { $$ = nullptr; }
         | main          { $$ = new mml::program_node(LINE, $1); }
         ;

main : tBEGIN decls_opt insts_opt tEND  { $$ = new mml::block_node(LINE, $2, $3); }
     ;

decls_opt : /*empty*/    { $$ = new cdk::sequence_node(LINE); }
          | decls        { $$ = $1; }
          ;

decls :       decl  { $$ = new cdk::sequence_node(LINE, $1); }
      | decls decl  { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

decl :          type  tIDENTIFIER               ';'      { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
     |          type  tIDENTIFIER '=' expr      ';'      { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
     |          tTYPE_AUTO tIDENTIFIER '=' expr ';'      { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); }
     | tFORWARD type  tIDENTIFIER               ';'      { $$ = new mml::variable_declaration_node(LINE, tFORWARD, $2, *$3, nullptr); }
     | tFOREIGN type  tIDENTIFIER               ';'      { $$ = new mml::variable_declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); }
     | tPUBLIC  type  tIDENTIFIER               ';'      { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, $2, *$3, nullptr); }
     | tPUBLIC  type  tIDENTIFIER '=' expr      ';'      { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, $2, *$3, $5); }
     | tPUBLIC             tIDENTIFIER '=' expr ';'      { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); }
     | tPUBLIC  tTYPE_AUTO tIDENTIFIER '=' expr ';'      { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$3, $5); }
     ;

insts_opt : /* empty */  { $$ = new cdk::sequence_node(LINE); }
          | insts        { $$ = $1;}
          ;

insts :       inst  { $$ = new cdk::sequence_node(LINE, $1); }
      | insts inst  { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

inst : expr ';'                    { $$ = $1; }
     | exprs             tPRINT    { $$ = new mml::print_node(LINE, $1, false); }
     | exprs             tPRINTLN  { $$ = new mml::print_node(LINE, $1, true); }
     | stop ';'                    { $$ = $1; }
     | next ';'                    { $$ = $1; }
     | return ';'                  { $$ = $1; }
     | inst_conditional            { $$ = $1; }
     | inst_iteration              { $$ = $1; }
     | block                       { $$ = $1; }
     ;

stop : tSTOP             { $$ = new mml::stop_node(LINE); }
     | tSTOP tINTEGER    { $$ = new mml::stop_node(LINE, $2); }
     ;

next : tNEXT             { $$ = new mml::next_node(LINE); }
     | tNEXT tINTEGER    { $$ = new mml::next_node(LINE, $2); }
     ;

return : tRETURN        { $$ = new mml::return_node(LINE, nullptr); }
       | tRETURN expr   { $$ = new mml::return_node(LINE, $2); }
       ;

inst_conditional : tIF '(' expr ')' inst               { $$ = new mml::if_node(LINE, $3, $5); }
                 | tIF '(' expr ')' inst if_false      { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
                 ;

if_false : tELSE inst                          { $$ = $2; }
         | tELIF '(' expr ')' inst             { $$ = new mml::if_node(LINE, $3, $5); }
         | tELIF '(' expr ')' inst if_false    { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
         ;

inst_iteration : tWHILE '(' expr ')' inst    { $$ = new mml::while_node(LINE, $3, $5); }
               ;

block : '{' decls_opt insts_opt '}'      { $$ = new mml::block_node(LINE, $2, $3); }
      ;

types_opt : /*empty*/    { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); }
          | types        { $$ = $1; }
          ;

types :           type        { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
      | types ',' type        { $$ = $1; $1->push_back($3); }
      ;

type : tTYPE_STRING           { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tTYPE_INT              { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tTYPE_DOUBLE           { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tTYPE_VOID             { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | '[' type ']'           { $$ = cdk::reference_type::create(4, $2); }
     | '[' tTYPE_AUTO ']'     { $$ = cdk::reference_type::create(4, nullptr); }
     | function_type          { $$ = $1; }
     ;

func_definition : '(' vars ')' tATR type block      { $$ = new mml::function_definition_node(LINE, $2, $6, $5); }
                ;

vars : /*empty*/         { $$ = new cdk::sequence_node(LINE); }
     |          var      { $$ = new cdk::sequence_node(LINE, $1); }
     | vars ',' var      { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

var : type tIDENTIFIER   { $$ = new mml::variable_declaration_node(LINE, 0, $1, *$2, nullptr); }
    ;

function_type : type '<' types_opt '>'  { $$ = cdk::functional_type::create(*$3, $1); }
              ;

exprs_opt : /* empty */  { $$ = new cdk::sequence_node(LINE); }
                | exprs  { $$ = new cdk::sequence_node(LINE, $1); }
                ;

exprs :           expr    { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

expr : func_definition            { $$ = $1; }
     | integer                    { $$ = $1; }
     | double                     { $$ = $1; }
     | string                     { $$ = new cdk::string_node(LINE, $1); }
     | tNULL                      { $$ = new mml::null_node(LINE); } 
     | lval                       { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr              { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | expr '+' expr              { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr              { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr              { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr              { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr              { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr  '<' expr             { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr tLE  expr             { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tEQ  expr             { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tGE  expr             { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr  '>' expr             { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tNE  expr             { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tAND  expr            { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR   expr            { $$ = new cdk::or_node (LINE, $1, $3); }
     | '-' expr %prec tUMINUS     { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUMINUS     { $$ = $2; }
     | '~' expr                   { $$ = new cdk::not_node(LINE, $2); }
     | expr '(' exprs_opt ')'     { $$ = new mml::function_call_node(LINE, $1, $3); }
     | '@' '(' exprs_opt ')'      { $$ = new mml::function_call_node(LINE, nullptr, $3); }
     | tSIZEOF '(' expr ')'       { $$ = new mml::sizeof_node(LINE, $3); }
     | '(' expr ')'               { $$ = $2; }
     | '[' expr ']'               { $$ = new mml::stack_alloc_node(LINE, $2); }
     | lval '?'                   { $$ = new mml::address_of_node(LINE, $1); }
     ;

lval : tIDENTIFIER                              { $$ = new cdk::variable_node(LINE, *$1); }
       | lval                   '[' expr ']'    { $$ = new mml::index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
       |      '(' expr ')'      '[' expr ']'    { $$ = new mml::index_node(LINE, $2, $5); }
       | expr '(' exprs_opt ')' '[' expr ']'    { $$ = new mml::index_node(LINE, new mml::function_call_node(LINE, $1, $3), $6); }
       ;         


integer : tINTEGER            { $$ = new cdk::integer_node(LINE, $1); };
double  : tDOUBLE             { $$ = new cdk::double_node(LINE, $1); };
string  : tSTRING             { $$ = $1; }
        | string tSTRING      { $$ = $1; $$->append(*$2); delete $2; }
        ;

%%