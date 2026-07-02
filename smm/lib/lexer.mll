{
open Parser

exception LexicalError of string

let comment_depth = ref 0
}

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start = parse
  | blank { start lexbuf }
  | "(*" { comment_depth :=1;
           comment lexbuf;
           start lexbuf }
  | number { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  (* | "and" { AND } *)
  (* | "or" { OR } *)
  | "not" { NOT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fn" { FN }
  | "let" { LET }
  | "in" { IN }
  | id { ID (Lexing.lexeme lexbuf) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "=" { EQUAL }
  | "<" { LB }
  (* | ";" { SEMICOLON } *)
  | "," { COMMA }
  (* | ".1" { PERIOD1 } *)
  (* | ".2" { PERIOD2 } *)
  | "=>" { ARROW }
  | ":=" { COLONEQ }
  | "(" { LP }
  | ")" { RP }
  | eof { EOF }
  | _ { raise (LexicalError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
and comment = parse
  | "(*" { comment_depth := !comment_depth+1; comment lexbuf}
  | "*)" { comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
  | eof { raise (LexicalError "Unterminated comment") }
  | _   { comment lexbuf }
