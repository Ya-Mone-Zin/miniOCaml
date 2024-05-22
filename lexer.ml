(* S8å®Ÿé¨“ç”¨ Lexer 2011/10/12 *)

{
open Parser
}

let space = [' ' '\t' '\n' '\r'] (*these are def of set of chars*)
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse (*these are list of tokens*)
  (* æ•´æ•°å®šæ•° *)
  | digit+ (*its a kind of regualar expression like 1+ 2+*) (*this is the rule for transforming integers  like "1" ==> INT(1)
               "123" ==> INT(123)*)(*role of lexer*)
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) } (*In this case integer 1 is like INT(1)*)
  
  (* æ¼”ç®—å­ *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | ';'       { SEMICOL }
  | "::"      { COLCOL }

  (* æ‹¬å¼§é¡ž *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }
  
  (* åŒºåˆ‡ã‚Šè¨˜å· *)
  | "->"      { ARROW }
  | '|'       { VBAR }
  
  (* ã‚­ãƒ¼ãƒ¯ãƒ¼ãƒ‰ *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { FUN }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "match"   { MATCH }
  | "with"    { WITH }
  | "List.hd" { HEAD }
  | "List.tl" { TAIL }

  (* å¤‰æ•° *)
  | alpha alnum* (* start with alpha and follow anything ::this is to transform the alphabet to variables like "a" ==> VAR("a") "v8" ==> VAR("v8")
                    "x_9_a" ==> VAR("x_9_a")*)
    { VAR (Lexing.lexeme lexbuf) }
  
  (* åˆ¶å¾¡è¨˜å· *)
  | eof       { EOF } (*end of file*)

  (* ã‚¹ãƒšãƒ¼ã‚¹ã‚’èª­ã¿é£›ã°ã™ *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }