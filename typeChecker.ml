(* Definition of the type of the expression to be processed *)
type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | Times of exp * exp 
  | Division of exp * exp 
  | Subtract of exp * exp 
  | GreaterThan of exp * exp (* Greater than comparison *)
  | LessThan of exp * exp    (* Less than comparison *)
  | If of exp * exp * exp
  | Eq of exp * exp 
  | Var of string  (* variable e.g. x  extending the variable in mini2OCaml*) 
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
  | Let of string * exp * exp (* let x = e1 in e2 *)
  | LetRec of string * string * exp * exp 
  
  (* Type definition representing ‘type’ *)
  type ty = TInt | TBool (*this is the def of type in mini2OCaml it has two types Int & bool*)|TArrow of ty(*type of argu*) * ty(*type of return value*) (*this is for function TArrow for two types*)
  type tyenv = (string * ty) list (*we need type environment to handle the var*)

  (* tcheck1 : exp -> ty(the return type of fun is the type of expression e) *)
(*let rec tcheck1(*this is the current typechecker accept argu e*) e(*this is an expression*) =
  match e with
  | IntLit(_)    -> TInt (*if its IntLit then the type of expression is TInt*)
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck1 e1, tcheck1 e2, tcheck1 e3) with
          (TBool,TInt,TInt) -> TInt(* for the return type we should care about e2 and e3 as long as it is a if expression e1 must be boolean because we need to know (true/false) to know then or else case and the type of the rest two should be the same if not it will happen type error*)
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) ->
      begin
        match (tcheck1 e1, tcheck1 e2) with 
        |  (TInt, TInt) -> TBool
        |  (TBool, TBool) -> TBool 
        | _ -> failwith "type error in Eq"
      end *)
    
      let emptyenv () = [] (*handle environment empty environment *)

      let ext(*extend*) env x v(*3 argus env variable and value*) = (x,v) :: env (*add variable x and its value v pair to env*)
                     
      let rec lookup x env =
        match env with
          | [] -> failwith ("unbound variable: " ^ x)
          | (y,v)::tl -> if x=y then v 
        else lookup x tl 

(* tcheck2 : tyenv -> exp -> ty *)
  let rec tcheck2 te e = (*it takes two argus type environment and argu expression return the type of input expression *)
    match e with
      | Var(s)    ->  lookup s te   (* 変数の型は、型環境 te から取ってくる *)
      | IntLit(_)    -> TInt (*if its IntLit then the type of expression is TInt*)
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end

  | Times(e1,e2) -> 
    begin
      match (tcheck2 te e1, tcheck2 te e2) with
        (TInt,TInt) -> TInt
      | _ -> failwith "type error in Times"
    end

  | Division(e1,e2) ->
    begin
      match (tcheck2 te e1, tcheck2 te e2) with
       | (TInt,TInt) -> TInt
       | _ -> failwith "type error in Division"
    end

  | Subtract(e1,e2) -> 
    begin 
      match (tcheck2 te e1, tcheck2 te e2) with 
      | (TInt, TInt) -> TInt 
      | _ -> failwith "type error in Subtract"
    end

  | GreaterThan(e1,e2) -> 
    begin
      match (tcheck2 te e1, tcheck2 te e2) with 
      | (TInt, TInt) -> TBool
      | _ -> failwith "type error in GreaterThan"
    end

  |LessThan(e1,e2) -> 
    begin
      match (tcheck2 te e1, tcheck2 te e2) with 
      | (TInt, TInt) -> TBool 
      | _ -> failwith "type error in LessThan"
    end

  | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
          (TBool,TInt,TInt) -> TInt(* for the return type we should care about e2 and e3 as long as it is a if expression e1 must be boolean because we need to know (true/false) to know then or else case and the type of the rest two should be the same if not it will happen type error*)
        | (TBool,TBool,TBool) -> TBool
        | (TBool,TArrow(t10, t11),TArrow(t12,t13)) -> if t10 = t12 && t11 = t13 then TArrow(t10,t11) else failwith "type error in If"
        | _ -> failwith "type error in IF"
      end

  | Eq(e1,e2) ->
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
        |  (TInt, TInt) -> TBool
        |  (TBool, TBool) -> TBool 
        | _ -> failwith "type error in Eq"
      end

  | Let(x,e1,e2) -> (*we have to check both variable and the body of the fun and compare these two if not same type error*)
    let tx = lookup x te in
    let t1 = tcheck2 te e1 in 
    if tx = t1 then 
         tcheck2 te e2
    else failwith "type error in Let"
       
  | Fun(x, e1) -> (*x is argu and e1 is the body of funciton*)
        let t1 = lookup x te in (*t1 is the type of argument*)
        let t2 = tcheck2 te e1 in (*t2 is the type of return value (expression)*)
           TArrow(t1,t2) (* the type of the whole fun is from t1 to t2*)
  | App(e1,e2) ->  (*e1 is function and e2 is argu*)
        let t1 = tcheck2 te e1 in (*t1 is the type of fun that should be TArrow(ty,ty)*)
        let t2 = tcheck2 te e2 in (*t2 is the type of argu*)
          begin
           match t1 with
	   | TArrow(t10,t11) -> if t2=t10 then t11 (*t10 is the type of argu and t11 is the type of the function body so t2 should be equal with t10 if so the type of the whole expression (the return type) should be t11*)
                             else failwith "type error in App"
	   | _ -> failwith "type error in App"
          end

  | LetRec(f,x,e1,e2) -> (*we need to check the type of f and x from environment*)
      let tf = lookup f te in
      let tx = lookup x te in
      let te1 = tcheck2 te e1 in
      begin 
        match tf with 
        |TArrow(t10,t11)(*t10 is the type of x and t11 is type of e1*) -> if tx = t10 && te1 = t11 then tcheck2 te e2 else failwith "type error in LetRec"
        | _ -> failwith "type error in LetRec1"
      end
          let env2 = emptyenv();;
          let env3 = ext env2 "x" TInt;;
          let test2 = App (Fun ("x", Plus (Var "x", IntLit 1)), IntLit 3);;
          tcheck2 env3 test2;;

      