// S8å®Ÿé¨“ç”¨ Parser 2011/10/12

%{
open Syntax
%}

// ãƒªãƒ†ãƒ©ãƒ«
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// æ¼”ç®—å­
%token PLUS     // '+' (*these are def of tokens*)
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token COLCOL   // "::"

// æ‹¬å¼§é¡ž
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

// åŒºåˆ‡ã‚Šè¨˜å·
%token ARROW    // "->"
%token VBAR     // '|'
%token SEMICOL  // ';'

// ã‚­ãƒ¼ãƒ¯ãƒ¼ãƒ‰
%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token MATCH    // "match"
%token WITH     // "with"
%token HEAD     // "List.hd"
%token TAIL     // "List.tl"

// åˆ¶å¾¡è¨˜å·
%token EOF 

// æ¼”ç®—å­å„ªå…ˆé †ä½ (å„ªå…ˆåº¦ã®ä½Žã„ã‚‚ã®ã»ã©å…ˆ)
%nonassoc IN ELSE ARROW WITH (*this is the priority of the operators*)(*associative properties*)
%left VBAR
%left SEMICOL
%left EQUAL GREATER LESS
%right COLCOL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
// æœ€å¾Œã«arg_exprã®ä¸€ç•ªå·¦ã®ãƒˆãƒ¼ã‚¯ãƒ³ã‚’ä¸¦ã¹ã‚‹
%left VAR INT TRUE FALSE LBRA LPAREN (*this is the highest priority*)

%start main (*all the rest part are transform the sequence of tokens to syntax tree*)
%type <Syntax.exp> main

%%

// é–‹å§‹è¨˜å·
main:
  | exp EOF
    { $1 }
;

// ãƒªã‚¹ãƒˆãƒªãƒ†ãƒ©ãƒ«
list_inner:
  | exp { Cons($1, Empty) }
  | exp SEMICOL { Cons($1, Empty) }
  | exp SEMICOL list_inner { Cons($1, $3) }

// é–¢æ•°ã®å¼•æ•°ã«ãªã‚Œã‚‹å¼
arg_exp:
  | VAR      (*transformation examples are VAR("x") ==> Var("x")
                                TRUE ==> BoolLit(true)
                                INT(10) ==> IntLit(10)*)
    { Var $1 }
    
  | INT
    { IntLit $1 }
    
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
  
  // ç©ºãƒªã‚¹ãƒˆ
  | LBRA RBRA { Empty }
  
  | LBRA list_inner RBRA { $2 }
  
  // æ‹¬å¼§ã§å›²ã¾ã‚ŒãŸå¼
  | LPAREN exp RPAREN
    { $2 }
;

// å¼
exp:
  | arg_exp
    { $1 }
    
  // é–¢æ•°é©ç”¨ (e1 e2)
  | exp arg_exp
    { App ($1, $2) }
  
  // ç¬¦å·ã®åè»¢ -e
  | MINUS exp %prec UNARY
    { Minus (IntLit 0, $2) }
  
  // e1 + e2
  | exp PLUS exp (*[T1; PLUS; T2] ==> Plus(e1, e2)*)
    { Plus ($1, $3) }
  
  // e1 - e2
  | exp MINUS exp
    { Minus ($1, $3) }
  
  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { Div ($1, $3) }
    
  // e1 = e2
  | exp EQUAL exp
    { Eq ($1, $3) }
  
  // e1 < e2
  | exp LESS exp
    { Less ($1, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { Greater ($1, $3) }
    
  // e1 :: e2
  | exp COLCOL exp
    { Cons ($1, $3) }
    
  // List.hd e
  | HEAD arg_exp
    { Head $2 }
    
  // List.tl e
  | TAIL arg_exp
    { Tail $2 }
  
  // fun x -> e
  | FUN VAR ARROW exp (*[FUN; VAR("x"); ARROW; T1] ==> Fun("x", e1)*)
    { Fun ($2, $4) }
  
  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp
    { Let ($2, $4, $6) }
  
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, $4, $6, $8) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }
  
  // match e with ...
  | MATCH exp WITH cases_rev
    { Match ($2, List.rev $4) }
  
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

// matchæ–‡ã®caseã®åˆ—
// æ³¨: yaccã§ã¯å·¦å†å¸°ã®ã»ã†ãŒã‚¹ã‚¿ãƒƒã‚¯æ¶ˆè²»é‡ãŒå°‘ãªã„ã€‚
cases_rev:
  | pattern ARROW exp
    { [($1, $3)] }
    
  | cases_rev VBAR pattern ARROW exp
    { ($3, $5) :: $1 }
;

// ãƒ‘ã‚¿ãƒ¼ãƒ³
pattern:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }
    
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
    
  | LBRA RBRA
    { Empty }
    
  | pattern COLCOL pattern
    { Cons ($1, $3) }
;