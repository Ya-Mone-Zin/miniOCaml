(*Syntax of this language*)
type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | Times of exp * exp 
  | Division of exp * exp 
  | Subtract of exp * exp 
  | GreaterThan of exp * exp (* Greater than comparison *)
  | LessThan of exp * exp    (* Less than comparison *)
  | Eq of exp * exp 
  | If of exp * exp * exp 
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
  | Let of string * exp * exp (* let x = e1 in e2 *)
  | LetRec of string * string * exp * exp 

(*type ty = TInt | TBool | TVar of string *)(*type definition TVar is the type variable*)
(* new_typevar : string -> ty *)
(*let new_typevar s = TVar("'" ^ s) *)(*creation of new type variable*)
(*most simplest expression for type inference*)
(* tinf1 : tyenv -> exp -> tyenv * ty *)

let emptyenv () = [] (*handle environment empty environment *)

    let ext(*extend*) env x v(*3 argus env variable and value*) = (x,v) :: env (*add variable x and its value v pair to env*)               
    let rec lookup x env =
      match env with
        | [] -> failwith ("unbound variable: " ^ x)
        | (y,v)::tl -> if x=y then v 
      else lookup x tl 

      let rec substitute tvar t te =  (*tvar is unknown type and t is exact type like TInt and te is evnrionment*)
      match te with
      | [] -> []
      | (x,t2)(*this is head the pair of var and its type*) :: te2(*this is tail the rest of the type environment*) -> 
          let t3 = (if t2 = tvar then t else t2) in (*if t2 is specific known type substitute it with exact type t or leave it itself*)
          (x,t3)(*now we got the exact type of var x it's in t3*) :: (substitute tvar t te2) (*make substitution for the rest of the environment*)
    
(*let rec tinf1 te e =
  match e with
  | IntLit(_)    -> (te, TInt) (*that will return type environment and type of the exprssion*)
  | BoolLit(_)  -> (te, TBool)

  | Var(s)    ->  
    (try 
      let t1 = lookup s te in
        (te, t1)
with Failure(_) ->   (* if var s is not in type environment te*)
        let tvar = new_typevar s in (*if so we have to create new type variable to introduce the variable to type enviroment because currently we don't know the type of s*)
        let te1 = ext te s tvar(*tvar is the type of s*) in (*extend the var and its type to environment*)
           (te1, tvar)) (*tvar is the type name because we dont know the exact type of the variable *)


  | Plus(e1,e2)  -> 
            let (te1, t1) = tinf1 te e1 in (*to make the type inference for e1 we need to give the type environmant as an argument because e1 may contain var*)
            let te2 =  (*make new type environment*)
              begin
               match t1 with
               | TInt -> te1 (*if t1(is the type of e1) is the type TInt then te2 is the smae as te1*)
               | TVar(s) -> substitute t1 TInt te1 (*if t1 is unknown we will substitute the type of t1 as TInt in the type environment te1*)
               | _ -> failwith "type error in Plus"
              end
            in 
            let (te3, t2) = tinf1 te2 e2 in
            let te4 = 
              begin
               match t2 with
               | TInt -> te3 
               | TVar(s) -> substitute t2 TInt te3
               | _ -> failwith "type error in Plus"
              end
            in 
              (te4, TInt)    (*finally return updated type environemnt and type of plus*)
              
  (* substitute : ty -> ty -> tyenv -> tyenv *)

  | If(e1,e2,e3) -> 
        let (te1, t1) = tinf1 te e1 in
        let te2 = 
          begin
           match t1 with
           | TBool -> te1
           | TVar(s) -> substitute t1 TBool te1
           | _ -> failwith "type error in IF"
          end
        in 
        let (te3, t2) = tinf1 te2 e2 in (* it's the case of if then else so the type of the first expression should be boolean and the type of the rest two should be the same that's why we do type infrerence for the rest two together*)
        let (te4, t3) = tinf1 te3 e3 in
          begin
           match (t2,t3) with
           | (TInt,TInt) -> (te4, TInt)
           | (TBool,TBool) -> (te4, TBool)
           | (TInt,TVar(_)) -> 
         let te5 = substitute t3 TInt te4 in (te5, TInt)
           | (TVar(_),TInt) -> 
         let te5 = substitute t2 TInt te4 in (te5, TInt)
           | (TBool,TVar(_)) ->
         let te5 = substitute t3 TBool te4 in (te5, TBool)
           | (TVar(_),TBool) -> 
         let te5 = substitute t2 TBool te4 in (te5, TBool)
           | (TVar(_),TVar(_)) -> (*even if the type of both are unknown the exact type of this type should be the same so we substitute t2 with t3*)
         let te5 = substitute t2 t3 te4 in (te5, t3) 
           | _ -> failwith "type error in IF"
          end *)

       (* let tenv1 = emptyenv();;
        let add = Plus(IntLit(1), Var("x"));;
        tinf1 tenv1 add;;
        let condition = If(Var("x"), IntLit(1), Var("s"));; 
        tinf1 tenv1 condition;;
        let con = If(BoolLit(true), IntLit(1), BoolLit(false));;
        tinf1 tenv1 con;;
        let con2 = If(Var("x"), IntLit(3), Var("x"));;
        tinf1 tenv1 con2;; *)
        (*unification definition*)
    type tyvar = string
    type ty = TInt | TBool | TArrow of ty * ty | TVar of tyvar
    type tyenv = (string * ty) list (*type environment with a pair of information of expresion var and type of var*)
    type tysubst = (tyvar * ty) list (*tyvar is typevariable and ty is the exact type of type variable getting by substitution*)

        (*unification function receives two types."unify" receives multiple pairs of two types as an argumant and performs unification multiple times*)
        (*unificaiton algorithm*)
        let rec occurs tx t = (*check if tx is a part of t *)
          if tx = t then true (*if these two are equal true*)
          else 
            match t with (*if t is a fun we have to check is tx is a part of argument type or a part of return type*)
            | TArrow(t1,t2) -> (occurs tx t1) || (occurs tx t2)
            | _ -> false

        (* subst_ty : tysubst -> ty -> ty *)
(* 代入thetaを型t に適用する *)
let rec subst_ty theta t = (*here will come the data with type so it will not go to subst_ty1 and will go directly to pattern matching*)
  let rec subst_ty1 theta1 s = (*here will come when the type is Var s *)
    match theta1 with (*we have to check is theta1 is empty or not if so just reply Var type s cuz we don't need to substitute*)
      |	[] -> TVar(s) 
      | (tx,t1):: theta2 ->  (*if not we will try to check all the pairs in list to know the exact type for Var type s *)
	  if tx = s then t1   (*theta is a list of pairs (type variable, the exact type for this var) so check the type var of a pair is same as Var s if so the return type for s is the exact type of this pair and we have to continue this work unitl we found it or at the end of the list*)
	  else subst_ty1 theta2 s
  in match t with (*we don't need to substitute when the type is Int or Bool but if t is fun type we need to sub argu type and return type*)
    |  TInt -> TInt
    | TBool -> TBool
    | TArrow(t2,t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
    | TVar(s) -> subst_ty1 theta s (* if t is Type var s we need to go to subst_ty1 function*)

(* subst_tyenv  : tysubst -> tyenv -> tyenv *)
(* 代入thetaを型環境 te に適用する *)
let subst_tyenv theta te = (*substitution theta to type environment*) (*type environment is a list of pairs (var,the type of that var)*)
  List.map (fun (x,t) -> (x, subst_ty theta t)) te (*all second part of pairs elements in type environment should be substituted by theta*)
(* List.mapは OCaml における リストに対する iterator である *)

(* subst_eq : tysubst -> (ty * ty) list -> (ty * ty) list *)
(* 代入thetaを型の等式のリスト eql に適用する *)
let subst_eql theta eql = (* is substitution theta to all pairs of types in list *)
  List.map (fun (t1,t2) -> (subst_ty theta t1, subst_ty theta t2)) (*subst_ty is substitution to one type so by using this fun we can sub all types of pairs in list can be substituted one by one*)
	   eql

(* compose_subst : tysubst -> tysubst -> tysubst *)
(* 2つの代入を合成した代入を返す。theta1 が「先」でtheta2が「後」である *)
let rec compose_subst theta2 theta1 = (*we are trying to compose theta2 to theta1*)
  let theta11 =  (*first of all we nee to update theta1*)
    List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1 (*theta2: [('ty, t1)]
    theta1: [('tx, t2)] 
    t2 = TArrow('ty, 'ty)
     TArrow(t1,t1) because the part t in theta 1 may also include type var *)
  in
    List.fold_left (fun tau -> fun (tx,t) -> (*tau is the first argu of this whole fun and pair of (tx,t) is the second argu of whole fun*)
		      try 
			let _ = lookup tx theta11 in (*tau is like x and (tx,t) is like a1 so a1 is an element of theta2 so we need to check is (tx,t) is already have in theta 11 or not*)
			  tau (*if we found we don't need to compose this pair to tau*)
		      with Failure(_) ->
			    (tx,t) :: tau) (*if not we need to append to tau*)
                   theta11 (*tau is smiliar with theta11 in some cases*)
                   theta2
(* List.fold_left は、リストに対する iterator であり、
 * List.fold_left f x [a1;a2;a3] は  (f (f (f x a1) a2) a3) を返す
 *)
    let unify eql =   (*eql is the list of pairs of two type*)(*unify fun accepts the list of two types as argu and return the type substitution*)
      let rec solve eql theta = (*if eql is empty we don't need to unify and return the current substitution phase*)(*theta is the return type of unify same as tysub*)
        match eql with
          | [] -> theta
          | (t1,t2):: eql2 ->
	          if t1 = t2 then solve eql2 theta
	          else 
              begin
                match (t1,t2) with
	                | (TArrow(t11,t12),TArrow(t21,t22))
	                    -> solve ((t11,t21)::(t12,t22)::eql2) theta
	                | (TVar(s), _)
	                    -> if (occurs t1 t2) then failwith "unification failed"
	                      else solve (subst_eql [(s,t2)] eql2) (*now we are considering the situtation of t1 is var s and t1 also didn't occour in t2 so we can unify t1 and t2 that means we can substitute s with t2 in the rest of the equation *)
	                      (compose_subst [(s,t2)] theta)
	                | (_,TVar(s))
	                    -> if (occurs t2 t1) then failwith "unification failed"
	                      else solve (subst_eql [(s,t1)] eql2)
	                      (compose_subst [(s,t1)] theta) (*so we found out the type of Var s if t1 so we need to give the pair of information to theta by composing *)
	                | (_,_) -> failwith "unification failed"
              end
      in solve eql [] (*empty theda*)

      let theta0 = ([] : tysubst) (*initial substitution with empty list*)

(* new_typevar : int -> ty * int ;; 型が間違っていたので修正 [2013/12/04] *)
(*this fun is to distinguish all type variables form others*)
let new_typevar n =  (*n is the argument for counter and +1 every time when we create new typevar and return a pair of new typevariable and incresed counter*)
  (TVar ("'a" ^ (string_of_int n)), n+1)

let rec remove te x = 
  match te with 
  |[] -> []
  |(y,t) :: tl -> if y = x then tl 
    else (y,t) :: remove tl x 

(* tinf2 : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec tinf2 te e n =
  match e with (*pattern match the expression*)
    | Var(s) -> (*if it is variable s*)
	(try
	   let t1 = lookup s te in (te, t1, theta0, n) (*find the type of this var s in type environment and if we found it put it to t1 and return*)
	 with Failure(_) ->                 (* te and t1 is the type of s and theta0 is initial substitution and n is current counter*)
	   let (tx,n1) = new_typevar n in (*if we can't find come to this part and create new typevar and return tx(new typevar) and increased counter*)
	   let te1 = ext te s tx in (*then extend the te with new typevar for var s and return updated env, typevar for s, substitution and increased counter *)
	     (te1, tx, theta0, n1))
    | IntLit(_)   -> (te, TInt, theta0, n)
    | BoolLit(_)  -> (te, TBool, theta0, n)
    | Plus(e1,e2) -> 
	    let (te1, t1, theta1, n1) = tinf2 te e1 n in
	    let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
	    let t11 = subst_ty theta2 t1 in
	    let theta3 = unify [(t11,TInt); (t2,TInt)] in
	    let te3 = subst_tyenv theta3 te2 in (*for this case te3 don't need to (we can) reflect to theta4 after this one because we use argu te2 this is reflec to theta 1 and 2 so its reflect to all theta and equal as theta4x*)
	    let theta4 = compose_subst theta3  (*theta 3 is less than theta4 so by using 3 this is more efficient*)
	              (compose_subst theta2 theta1) in
	    (te3, TInt, theta4, n2) 

    | Times(e1,e2) ->
	      let (te1, t1, theta1, n1) = tinf2 te e1 n in
	      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in 
        let theta3 = unify [(t11,TInt); (t2,TInt)] in 
        let te3 = subst_tyenv theta3 te2 in 
        let theta4 = compose_subst theta3 
                    (compose_subst theta2 theta1) in 
          (te3, TInt, theta4, n2)

    | Division(e1,e2) ->
	      let (te1, t1, theta1, n1) = tinf2 te e1 n in
	      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in 
        let theta3 = unify [(t11,TInt); (t2,TInt)] in 
        let te3 = subst_tyenv theta3 te2 in 
        let theta4 = compose_subst theta3 
                    (compose_subst theta2 theta1) in 
          (te3, TInt, theta4, n2)

    | Subtract(e1,e2) ->
        let (te1, t1, theta1, n1) = tinf2 te e1 n in
        let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in 
        let theta3 = unify [(t11,TInt); (t2,TInt)] in 
        let te3 = subst_tyenv theta3 te2 in 
        let theta4 = compose_subst theta3 
                        (compose_subst theta2 theta1) in 
          (te3, TInt, theta4, n2)

    | GreaterThan(e1,e2) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 
                      (compose_subst theta2 theta1) in
       (te3, TBool, theta4, n2)

    | LessThan(e1,e2) -> 
        let (te1, t1, theta1, n1) = tinf2 te e1 n in
        let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [(t11,TInt); (t2,TInt)] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 
                        (compose_subst theta2 theta1) in
         (te3, TBool, theta4, n2)

    | Eq(e1,e2) -> 
          let (te1, t1, theta1, n1) = tinf2 te e1 n in
          let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
          let t11 = subst_ty theta2 t1 in
          let theta3 = unify [(t11,t2)] in
          let te3 = subst_tyenv theta3 te2 in
          let theta4 = compose_subst theta3 
                          (compose_subst theta2 theta1) in
           (te3, TBool, theta4, n2)

    | Let(x,e1,e2) -> (*x is new var here*)
      let (tx,n1) = new_typevar n in
      let te1 = ext te x tx in
      let (te2, t1, theta1, n2) = tinf2 te1 e1 n1 in
      let (te3, t2, theta2, n3) = tinf2 te2 e2 n2 in 
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,tx)] in
      let t12 = subst_ty theta3 t2 in
      let te4 = subst_tyenv theta3 te3 in 
      let theta4 = compose_subst theta3 
                      (compose_subst theta2 theta1) in
        (te4, t12, theta4, n3)

    | LetRec(f,x,e1,e2) -> 
      let (tx,n1) = new_typevar n in 
      let te1 = ext te x tx in 
      let (tf,n2) = new_typevar n1 in 
      let te2 = ext te1 f tf in 
      let (te3, t1, theta1, n3) = tinf2 te2 e1 n2 in
      let (te4, t2, theta2, n4) = tinf2 te3 e2 n3 in 
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(tf,TArrow(tx,t11))] in
      let t12 = subst_ty theta3 t2 in
      let te5 = subst_tyenv theta3 te4 in
      let theta4 = compose_subst theta3 
                      (compose_subst theta2 theta1) in 
      (te5, t12, theta4, n4)
      
    | If(e1,e2,e3) ->  
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (te3, t3, theta3, n3) = tinf2 te2 e3 n2 in 
      let t11  = subst_ty theta2 t1 in 
      let t111 = subst_ty theta3 t11 in 
      let t12  = subst_ty theta3 t2 in 
      let theta4 = unify [(t111,TBool); (t12,t3)] in
      let tf = subst_ty theta4 t12 in
      let te4 = subst_tyenv theta4 te3 in 
      let theta5 = compose_subst theta4 
                      (compose_subst theta3 (compose_subst theta2 theta1)) in
      (te4, tf, theta5, n3)

    | Fun(x,e) -> 
	    let (tx,n1) = new_typevar n in (*first we create the new typevar for x*)
	    let te1 = ext te x tx in
	    let (te2, t1, theta1, n2) = tinf2 te1 e n1 in
	    let t2 = subst_ty theta1 tx in
  (*we need to remove the information of x from environment because this information is only valid within this scope of the body of the function e so it shouldn't be appear in any other place that's why we remove it after using*)
	    let te3 = remove te2 x in (*the return type of this function is type environment which removed the pair of x and its type eg  te2 = [("x13",TInt); ("x20",TBool)] (remove te2 "x13") = [("x20",TBool)]*)
	    (te3, TArrow(t2, t1), theta1, n2)
    | App(e1,e2) -> (*e1 is fun and e2 is argu of e1*)
	    let (te1, t1, theta1, n1) = tinf2 te e1 n in
	    let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
	    let (tx,n3) = new_typevar n2 in (*we create the new typevar for the result of this fun application because we don't know the type of this part yet*)
	    let t11 = subst_ty theta2 t1 in
	    let theta3 = unify [(t11,TArrow(t2,tx))] in
	    let t3 = subst_ty theta3 tx in
	    let te3 = subst_tyenv theta3 te2 in
	    let theta4 = compose_subst theta3 
	              (compose_subst theta2 theta1) in

	    (te3, t3, theta4, n3)
      | _ -> failwith "unknown expression"

(* 以下は例題 *)
let tinf2top e = tinf2 [] e 0
let _ = tinf2top (If(BoolLit(true), IntLit(1), IntLit(100)))
let _ = tinf2top (If(Var("x"),Plus(Var("y"), IntLit(10)), IntLit(100)))
let _ = tinf2top (If(Var("x"),Plus(Var("y"), Var("z")), Var("z")))
let _ = tinf2top (Var("x"))