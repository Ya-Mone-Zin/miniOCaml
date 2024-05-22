(* main.ml *)

(* æ§‹æ–‡å®šç¾©ãƒ•ã‚¡ã‚¤ãƒ« syntax.ml ã§å®šç¾©ã•ã‚ŒãŸ expåž‹ã‚’ä½¿ã† *)
open Syntax ;;

(* ä¸Žãˆã‚‰ã‚ŒãŸæ–‡å­—åˆ—ã®å­—å¥è§£æžã¨æ§‹æ–‡è§£æžã ã‘ã‚’è¡Œã†é–¢æ•° *)
(* parse : string -> exp *)

let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

(* ä½¿ç”¨ä¾‹ã¯ä»¥ä¸‹ã®é€šã‚Šã€‚parseé–¢æ•°ã¯ Mainãƒ¢ã‚¸ãƒ¥ãƒ¼ãƒ«ã«ã¯ã„ã£ã¦ã„ã‚‹ã®ã§
   open Main;; parse "...";; ã¨ã™ã‚‹ã‹ Main.parse "...";;
   ã¨ã—ã¦å‘¼ã³ã ã™ã€‚

$ ./miniocaml
      Objective Caml version 3.09.1

# Main.parse "let x = 3 + 1 * 4 in fun y -> x + y";;
- : Syntax.exp =
Syntax.Let ("x",
 Syntax.Plus (Syntax.IntLit 3,
  Syntax.Times (Syntax.IntLit 1, Syntax.IntLit 4)),
 Syntax.Fun ("y", Syntax.Plus (Syntax.Var "x", Syntax.Var "y")))
#  Main.parse "1 + 2 * 3 ";;
- : Syntax.exp =
Syntax.Plus (Syntax.IntLit 1,
 Syntax.Times (Syntax.IntLit 2, Syntax.IntLit 3))
#
*)