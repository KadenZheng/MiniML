(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Float of float                       (* floats *)
  | String of string                     (* strings *)  
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ | Bool _ | Float _ | String _ | Raise | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> SS.union (free_vars e1) 
    (SS.union (free_vars e2) (free_vars e3))
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2))
  | Letrec (v, e1, e2) -> 
    SS.union (SS.remove v (free_vars e2)) (SS.remove v (free_vars e1))
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
(* Inspiration taken from gensym *)
let new_varname : unit -> varid =
  let counter = ref 0 in
  fun () ->
    let varname = "var" ^ (string_of_int !counter) in
    counter := !counter + 1;
  varname ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec subst' (exp : expr) : expr =
    match exp with
    | Var v -> if v = var_name then repl else exp
    | Num _ | Bool _ | Float _ | String _ | Raise | Unassigned -> exp
    | Unop (op, expr) -> Unop (op, subst' expr)
    | Binop (op, expr1, expr2) -> Binop (op, subst' expr1, subst' expr2)
    | Conditional (e1, e2, e3) -> Conditional (subst' e1, subst' e2, subst' e3)
    | Fun (v, expr) ->
        if v = var_name then exp (* Avoid capturing the subst. var  *)
        else
          let frees = free_vars repl in
          if SS.mem v frees then
            (* Renaming the func parameter to avoid variable capture *)
            let z = new_varname () in
            Fun (z, subst' (subst v (Var z) expr))
          else
            Fun (v, (subst' expr))
    | Let (v, expr1, expr2) ->
        if v = var_name then
          Let (v, subst' expr1, expr2)
        else
          let frees = free_vars repl in
          if SS.mem v frees then
            (* Renaming the let-bound variable to avoid variable capture *)
            let z = new_varname () in
            Let (z, subst' (subst v (Var z) expr1), subst' (subst v (Var z) expr2))
          else
            Let (v, subst' expr1, subst' expr2)
    | Letrec (v, e1, e2) ->
        if v = var_name then exp
        else
          let frees = free_vars repl in
          if SS.mem v frees then
            (* Rename the letrec-bound variable to avoid variable capture *)
            let z = new_varname () in
            Letrec (z, subst' (subst v (Var z) e1), subst' (subst v (Var z) e2))
          else
            Letrec (v, subst' e1, subst' e2)
    | App (e1, e2) -> App (subst' e1, subst' e2)
  in
  subst' exp
;;

  
(*......................................................................
  String representations of expressions
 *)


(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let unop_prefix_conc (unop : unop) : string =
  match unop with
  | Negate -> "~-"
;;

let binop_prefix_conc (binop : binop) : string =
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<"
  | FPlus -> "+."
  | FMinus -> "-."
  | FTimes -> "*."
  | FDivide -> "/."
;;

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | String s -> s
  | Unop (op, e) ->
      let op_str = unop_prefix_conc op in
      op_str ^ "(" ^ exp_to_concrete_string e ^ ")"
  | Binop (op, e1, e2) ->
      let op_str = binop_prefix_conc op in
      "(" ^ exp_to_concrete_string e1 ^ " " ^ op_str ^ " " ^ exp_to_concrete_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
      "if " ^ exp_to_concrete_string e1 ^ " then " ^ exp_to_concrete_string e2 ^ " else " ^ exp_to_concrete_string e3
  | Fun (v, e) ->
      "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | Let (v, e1, e2) ->
      "let " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) ->
      "let rec " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
      exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2
;;

let unop_prefix (unop : unop) : string =
  match unop with
  | Negate -> "Negate"
;;

let binop_prefix (binop : binop) : string =
  match binop with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan"
  | FPlus -> "FPlus"
  | FMinus -> "FMinus"
  | FTimes -> "FTimes"
  | FDivide -> "FDivide"
;;
   

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
  
  let rec exp_to_abstract_string (exp : expr) : string =
    match exp with
    | Var v -> "Var(" ^ v ^ ")"
    | Num n -> "Num(" ^ string_of_int n ^ ")"
    | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
    | Float f -> "Float(" ^ string_of_float f ^ ")"
    | String s -> "String(\"" ^ s ^ "\")"
    | Unop (op, e) ->
        let op_str = unop_prefix op in
        "Unop(" ^ op_str ^ ", " ^ exp_to_abstract_string e ^ ")"
    | Binop (op, e1, e2) ->
        let op_str = binop_prefix op in
        "Binop(" ^ op_str ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Conditional (e1, e2, e3) ->
        "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
    | Fun (v, e) ->
        "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
    | Let (v, e1, e2) ->
        "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Letrec (v, e1, e2) ->
        "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (e1, e2) ->
        "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  ;;