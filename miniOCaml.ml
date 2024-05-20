(* Definition of the type of expression to be processed *)
type exp =
  | IntLit of int            (* Integer literal *)
  | Plus of exp * exp        (* Addition *)
  | Times of exp * exp       (* Multiplication *)
  | BoolLit of bool        (* 追加分; 真理値リテラル, つまり trueや false  *)
  | If of exp * exp * exp  (* 追加分; if-then-else式 *)
  | Eq of exp * exp        (* 追加分; e1 = e2 *)
  | Division of exp * exp    (* Division *)
  | Subtract of exp * exp    (* Subtraction *)
  | GreaterThan of exp * exp (* Greater than comparison *)
  | LessThan of exp * exp    (* Less than comparison *)
  | Var of string            (*variable*)
  | Let of string * exp * exp (* let x = e1 in e2 *)
  | Fun of string * exp
  | App of exp * exp
  | LetRec of string * string * exp * exp 

  type value =
  | IntVal of int
  | BoolVal of bool
  | ListVal of value list 
  | FunVal  of string * exp * ((string * value) list)
  | RecFunVal  of string * string * exp * ((string * value) list)

  let rec eval1 e =
    match e with
    | IntLit(n) -> IntVal(n)
    | Plus(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> IntVal(n1 + n2)
        | _ -> failwith "Integer values expected")
    | Times(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> IntVal(n1 * n2)
        | _ -> failwith "Integer values expected")
    | Division(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> if n2 != 0 then IntVal(n1 / n2) else failwith "Division by zero"
        | _ -> failwith "Integer values expected")
    | Subtract(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> IntVal(n1 - n2)
        | _ -> failwith "Integer values expected")
    | GreaterThan(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 > n2)
        | _ -> failwith "Integer values expected")
    | LessThan(e1, e2) -> 
        (match (eval1 e1, eval1 e2) with
        | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 < n2)
        | _ -> failwith "Integer values expected")
    | Eq(e1,e2) ->
      begin
	    match (eval1 e1, eval1 e2) with
	      |   (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
	      |   (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
	      |   _ -> failwith "wrong value"
      end
    | BoolLit(b) -> BoolVal(b)
    | If(e1(*condition*),e2,e3) ->
      begin
	    match (eval1 e1(*if e1 is true then evaluate e2*)) with
	      | BoolVal(true) -> eval1 e2
	      | BoolVal(false) -> eval1 e3
	      | _ -> failwith "wrong value"
      end
        | _ -> failwith "unknown expression"
  
        let e1 = GreaterThan(IntLit(5), IntLit(3))  (* should return BoolVal(true) *)
        let e2 = LessThan(IntLit(0), IntLit(1))     (* should return BoolVal(true) *)
        let _ = eval1 (Eq (IntLit 1, IntLit 1))
        let _ = eval1 (If (Eq(IntLit 2, IntLit 11),
                   Times(IntLit 1, IntLit 2),
                   Times(IntLit 1, Plus(IntLit 2,IntLit 3))))

  (*In the programming language world, the term ‘environment’ refers to the 
  mapping information of what values each variable has.*)
  (*The environment required by the processing system of a programming language 
  is a ‘single set of data’ that contains information such as ‘variables x, y, z, ... 
  have the values 1, true, 13, ... respectively’.*) 
  (*There is no upper limit to the number of variables that an environment can have, 
  it is decided to represent environments using lists, which are data structures with no 
  upper limit to their length.*)
    let emptyenv () = [] (*handle environment empty environment *)

    let ext(*extend*) env x v(*3 argus env variable and value*) = (x,v) :: env (*add variable x and its value v pair to env*)               
    let rec lookup x env =
      match env with
        | [] -> failwith ("unbound variable: " ^ x)
        | (y,v)::tl -> if x=y then v 
      else lookup x tl 
  (* eval2b : exp -> value *)
  let rec eval2b e =
  let binop f e1 e2 =
    match (eval2b e1, eval2b e2) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in 
  match e with
  | IntLit(n)    -> IntVal(n)
  | Plus(e1,e2)  -> binop (+) e1 e2
  | Times(e1,e2) -> binop ( * ) e1 e2
  | _ -> failwith "unknown expression"

  let rec eval3 e env =           
    match e with
    | Var(x)       -> lookup x env 
    | Let(x,e1,e2) ->
        let env1 = ext env x (eval3 e1 env) 
        in eval3 e2 env1
    | _ -> failwith "unknown expression"

    let rec eval4 e env =
      match e with
      | Fun(x,e1) -> FunVal(x, e1, env)
      | App(e1,e2) ->
        let funpart = (eval4 e1 env) in
        let arg = (eval4 e2 env) in
          begin
           match funpart with
           | FunVal(x,body,env1(*past environment*)) ->
              let env2 = (ext env1 x arg) in
              eval4 body env2
           | RecFunVal(f,x,body,env1) ->
              let env2 = (ext (ext env1 x arg) f funpart) in
              eval4 body env2
           | _ -> failwith "wrong value in App"
          end
      | LetRec(f,x,e1,e2) ->
        let env1 = ext env f (RecFunVal (f, x, e1, env))
        in eval4 e2 env1
      | _ -> failwith "unknown expression" 

    let rec eval5 e env =
      match e with
        | IntLit(n) -> IntVal(n)
        | Plus(e1, e2) -> 
          (match (eval5 e1 env, eval5 e2 env) with
          | (IntVal(n1), IntVal(n2)) -> IntVal(n1 + n2)
          | _ -> failwith "Integer values expected")
        | Times(e1, e2) -> 
          (match (eval5 e1 env, eval5 e2 env) with
          | (IntVal(n1), IntVal(n2)) -> IntVal(n1 * n2)
          | _ -> failwith "Integer values expected")
        | Division(e1, e2) -> 
          (match (eval5 e1 env, eval5 e2 env) with
            | (IntVal(n1), IntVal(n2)) -> if n2 != 0 then IntVal(n1 / n2) else failwith "Division by zero"
            | _ -> failwith "Integer values expected")
        | Subtract(e1, e2) -> 
            (match (eval5 e1 env, eval5 e2 env) with
            | (IntVal(n1), IntVal(n2)) -> IntVal(n1 - n2)
            | _ -> failwith "Integer values expected")
        | GreaterThan(e1, e2) -> 
          (match (eval5 e1 env, eval5 e2 env) with
          | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 > n2)
          | _ -> failwith "Integer values expected")
        | LessThan(e1, e2) -> 
          (match (eval5 e1 env, eval5 e2 env) with
          | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 < n2)
          | _ -> failwith "Integer values expected")
        | Eq(e1,e2) ->
            begin
            match (eval5 e1 env, eval5 e2 env) with
              |   (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
              |   (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
              |   _ -> failwith "wrong value"
            end
        | BoolLit(b) -> BoolVal(b)
        | If(e1(*condition*),e2,e3) ->
            begin
            match (eval5 e1(*if e1 is true then evaluate e2*)env) with
              | BoolVal(true) -> eval5 e2 env
              | BoolVal(false) -> eval5 e3 env
              | _ -> failwith "wrong value"
            end
        |Var(x)    -> lookup x env
        |Let(x,e1,e2) -> 
           let env1 = ext env x(eval5 e1 env)
           in eval5 e2 env1 
        | Fun(x, e1) -> FunVal(x, e1, env)
        | App(e1,e2) ->
          let funpart = (eval5 e1 env) in
          let arg = (eval5 e2 env) in
            begin
             match funpart with
             | FunVal(x,body,env1) ->
                let env2 = (ext env1 x arg) in
                eval5 body env2
             | RecFunVal(f,x,body,env1) ->
                let env2 = (ext (ext env1 x arg) f funpart) in
                eval5 body env2
             | _ -> failwith "wrong value in App"
            end
        | LetRec(f, x, e1, e2) ->
          let env1 = ext env f (RecFunVal(f, x, e1, env)) in
          eval5 e2 env1
      let rec5 = LetRec ("fact", "x", If(Eq(Var("x"), IntLit(0)), IntLit(1), Times(Var("x"), App(Var("fact"), Subtract(Var("x"), IntLit(1))))), App(Var("fact"), IntLit(5)))
      