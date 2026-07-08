%{
exception ParsingError of string
%}

%token <int> NUM
%token TRUE FALSE FN
%token <string> ID
// %token PLUS MINUS STAR SLASH PERCENT EQUAL LB SEMICOLON COMMA PERIOD1 PERIOD2 ARROW COLONEQ
%token PLUS MINUS STAR SLASH PERCENT EQUAL LB COMMA ARROW COLONEQ
// %token AND OR NOT IF THEN ELSE LET IN
%token NOT IF THEN ELSE LET IN
%token LP RP
%token EOF

%nonassoc IN
// %left SEMICOLON
%nonassoc THEN
%nonassoc ELSE
// %left OR
// %left AND
%left EQUAL LB
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
// %left PERIOD1 PERIOD2

%start program
%type <Smm.Smm_pre2.exp> program

%%

program:
    expr EOF { $1 }
  ;

expr:
    LP expr RP { $2 }
  | MINUS NUM { Smm.Smm_pre2.NUM (-$2) }
  | NUM { Smm.Smm_pre2.NUM ($1) }
  | TRUE { Smm.Smm_pre2.TRUE }
  | FALSE { Smm.Smm_pre2.FALSE }
  | ID { Smm.Smm_pre2.VAR ($1) }
  | expr PLUS expr { Smm.Smm_pre2.ADD ($1, $3) }
  | expr MINUS expr  {Smm.Smm_pre2.SUB ($1, $3) }
  | expr STAR expr { Smm.Smm_pre2.MUL ($1, $3) }
  | expr SLASH expr { Smm.Smm_pre2.DIV ($1, $3) }
  | expr PERCENT expr { Smm.Smm_pre2.MOD ($1, $3) }
  | expr EQUAL expr { Smm.Smm_pre2.EQUAL ($1, $3) }
  | expr LB expr { Smm.Smm_pre2.LESS ($1, $3) }
  // | expr AND expr { Smm.Smm_pre2.ADD ($1, $3) }
  // | expr OR expr { Smm.Smm_pre2.OR ($1, $3) }
  | NOT expr { Smm.Smm_pre2.NOT ($2) }
  // | expr SEMICOLON expr { Smm.Smm_pre2.SEQ ($1, $3) }
  // | expr COMMA expr { Smm.Smm_pre2.PAIR ($1, $3) }
  // | expr PERIOD1 { Smm.Smm_pre2.PFST ($1) }
  // | expr PERIOD2 { Smm.Smm_pre2.PSND ($1) }
  | IF expr THEN expr ELSE expr { Smm.Smm_pre2.IF ($2, $4, $6) }
  | ID LP vars RP { Smm.Smm_pre2.CALL ($1, $3) }
  | LET ID COLONEQ expr IN expr { Smm.Smm_pre2.LET ($2, $4, $6) }
  | LET FN ID LP vars RP ARROW expr IN expr { Smm.Smm_pre2.LETFN ($3, $5, $8, $10) }
  ;
vars:
    separated_list(COMMA, ID) { $1 }
	;
%%
