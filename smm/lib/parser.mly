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
%type <Smm.SmmPre.exp> program

%%

program:
    expr EOF { $1 }
  ;

expr:
    LP expr RP { $2 }
  | MINUS NUM { Smm.SmmPre.NUM (-$2) }
  | NUM { Smm.SmmPre.NUM ($1) }
  | TRUE { Smm.SmmPre.TRUE }
  | FALSE { Smm.SmmPre.FALSE }
  | ID { Smm.SmmPre.VAR ($1) }
  | expr PLUS expr { Smm.SmmPre.ADD ($1, $3) }
  | expr MINUS expr  {Smm.SmmPre.SUB ($1, $3) }
  | expr STAR expr { Smm.SmmPre.MUL ($1, $3) }
  | expr SLASH expr { Smm.SmmPre.DIV ($1, $3) }
  | expr PERCENT expr { Smm.SmmPre.MOD ($1, $3) }
  | expr EQUAL expr { Smm.SmmPre.EQUAL ($1, $3) }
  | expr LB expr { Smm.SmmPre.LESS ($1, $3) }
  // | expr AND expr { Smm.SmmPre.ADD ($1, $3) }
  // | expr OR expr { Smm.SmmPre.OR ($1, $3) }
  | NOT expr { Smm.SmmPre.NOT ($2) }
  // | expr SEMICOLON expr { Smm.SmmPre.SEQ ($1, $3) }
  // | expr COMMA expr { Smm.SmmPre.PAIR ($1, $3) }
  // | expr PERIOD1 { Smm.SmmPre.PFST ($1) }
  // | expr PERIOD2 { Smm.SmmPre.PSND ($1) }
  | IF expr THEN expr ELSE expr { Smm.SmmPre.IF ($2, $4, $6) }
  | ID LP vars RP { Smm.SmmPre.CALL ($1, $3) }
  | LET ID COLONEQ expr IN expr { Smm.SmmPre.LET ($2, $4, $6) }
  | LET FN ID LP vars RP ARROW expr IN expr { Smm.SmmPre.LETFN ($3, $5, $8, $10) }
  ;
vars:
    separated_list(COMMA, ID) { $1 }
	;
%%
