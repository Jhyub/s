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

// %right COMMA
%nonassoc ARROW
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
%type <Smm.Smm.exp> program

%%

program:
    expr EOF { $1 }
  ;

expr:
    LP expr RP { $2 }
  | MINUS NUM { Smm.Smm.NUM (-$2) }
  | NUM { Smm.Smm.NUM ($1) }
  | TRUE { Smm.Smm.TRUE }
  | FALSE { Smm.Smm.FALSE }
  | ID { Smm.Smm.VAR ($1) }
  | expr PLUS expr { Smm.Smm.ADD ($1, $3) }
  | expr MINUS expr  {Smm.Smm.SUB ($1, $3) }
  | expr STAR expr { Smm.Smm.MUL ($1, $3) }
  | expr SLASH expr { Smm.Smm.DIV ($1, $3) }
  | expr PERCENT expr { Smm.Smm.MOD ($1, $3) }
  | expr EQUAL expr { Smm.Smm.EQUAL ($1, $3) }
  | expr LB expr { Smm.Smm.LESS ($1, $3) }
  // | expr AND expr { Smm.Smm.ADD ($1, $3) }
  // | expr OR expr { Smm.Smm.OR ($1, $3) }
  | NOT expr { Smm.Smm.NOT ($2) }
  // | expr SEMICOLON expr { Smm.Smm.SEQ ($1, $3) }
  // | expr COMMA expr { Smm.Smm.PAIR ($1, $3) }
  // | expr PERIOD1 { Smm.Smm.PFST ($1) }
  // | expr PERIOD2 { Smm.Smm.PSND ($1) }
  | IF expr THEN expr ELSE expr { Smm.Smm.IF ($2, $4, $6) }
  | FN LP vars RP ARROW expr { Smm.Smm.FN ($3, $6) }
  | expr LP vars RP { Smm.Smm.CALL ($1, $3) }
  | LET ID COLONEQ expr IN expr { Smm.Smm.LET ($2, $4, $6) }
  ;
exprs:
    separated_list(COMMA, expr) { $1 }
	;
vars:
    separated_list(COMMA, ID) { $1 }
	;
%%
