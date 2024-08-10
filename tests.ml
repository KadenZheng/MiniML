open CS51Utils ;;
open Absbook ;; 
open Expr ;;
open Evaluation ;;
open Evaluation.Env ;;
open Miniml ;; 

let exp_to_concrete_string_test () =
  print_endline "BEGINNING OF EXP_TO_CONCRETE_STRING_TEST!!" ;
  unit_test (exp_to_concrete_string (Var "x") = "x") "exp_to_concrete_string Var";
  unit_test (exp_to_concrete_string (Num 42) = "42") "exp_to_concrete_string Num";
  unit_test (exp_to_concrete_string (Bool true) = "true") "exp_to_concrete_string Bool";
  unit_test (exp_to_concrete_string (Unop (Negate, Var "x")) = "~-(x)") "exp_to_concrete_string Unop";
  unit_test (exp_to_concrete_string (Binop (Plus, Var "x", Var "y")) = "(x + y)") "exp_to_concrete_string Binop";
  unit_test (exp_to_concrete_string (Conditional (Var "x", Var "y", Var "z")) = "if x then y else z") 
    "exp_to_concrete_string Conditional";
  unit_test (exp_to_concrete_string (Fun ("x", Binop (Plus, Var "x", Num 1))) = "fun x -> (x + 1)") 
    "exp_to_concrete_string Fun";
  unit_test (exp_to_concrete_string (Let ("x", Num 1, Binop (Plus, Var "x", Num 2))) = "let x = 1 in (x + 2)") 
    "exp_to_concrete_string Let";
  unit_test (exp_to_concrete_string 
               (Letrec ("fact", 
                        Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), Num 1, Binop (Times, Var "n", 
                        App (Var "fact", Binop (Minus, Var "n", Num 1))))), App (Var "fact", Num 5))) =
             "let rec fact = fun n -> if (n = 0) then 1 else (n * fact (n - 1)) in fact 5")
    "exp_to_concrete_string Letrec";
  unit_test (exp_to_concrete_string Raise = "raise") "exp_to_concrete_string Raise";
  unit_test (exp_to_concrete_string Unassigned = "unassigned") "exp_to_concrete_string Unassigned";
  unit_test (exp_to_concrete_string (App (Var "f", Var "x")) = "f x") "exp_to_concrete_string App";
  unit_test (exp_to_concrete_string (Float 3.14) = "3.14") "exp_to_concrete_string Float";
  unit_test (exp_to_concrete_string (Binop (FPlus, Float 5.55555, Float 1.5)) = "(5.55555 +. 1.5)") "exp_to_concrete_string Binop FPlus";
  unit_test (exp_to_concrete_string (Binop (FMinus, Float 4., Float 1.5)) = "(4. -. 1.5)") "exp_to_concrete_string Binop FMinus";
  unit_test (exp_to_concrete_string (Binop (FTimes, Float 2., Float 3.)) = "(2. *. 3.)") "exp_to_concrete_string Binop FTimes";
  unit_test (exp_to_concrete_string (Binop (FDivide, Float 1234., Float 2.)) = "(1234. /. 2.)") "exp_to_concrete_string Binop FDivide";
;;

let exp_to_abstract_string_test () =
  print_endline "BEGINNING OF EXP_TO_ABSTRACT_STRING_TEST!!" ;
  unit_test (exp_to_abstract_string (Var "x") = "Var(x)") "exp_to_abstract_string Var";
  unit_test (exp_to_abstract_string (Num 42) = "Num(42)") "exp_to_abstract_string Num";
  unit_test (exp_to_abstract_string (Bool true) = "Bool(true)") "exp_to_abstract_string Bool";
  unit_test (exp_to_abstract_string (Unop (Negate, Var "x")) = "Unop(Negate, Var(x))") "exp_to_abstract_string Unop";
  unit_test (exp_to_abstract_string (Binop (Plus, Var "x", Var "y")) = "Binop(Plus, Var(x), Var(y))") "exp_to_abstract_string Binop";
  unit_test (exp_to_abstract_string (Conditional (Var "x", Var "y", Var "z")) = "Conditional(Var(x), Var(y), Var(z))") 
    "exp_to_abstract_string Conditional";
  unit_test (exp_to_abstract_string (Fun ("x", Binop (Plus, Var "x", Num 1))) = "Fun(x, Binop(Plus, Var(x), Num(1)))") 
    "exp_to_abstract_string Fun";
  unit_test (exp_to_abstract_string (Let ("x", Num 1, Binop (Plus, Var "x", Num 2))) = "Let(x, Num(1), Binop(Plus, Var(x), Num(2)))") 
    "exp_to_abstract_string Let";
  unit_test (exp_to_abstract_string 
               (Letrec ("fact", Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), Num 1, Binop (Times, 
               Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))), App (Var "fact", Num 5))) =
             "Letrec(fact, Fun(n, Conditional(Binop(Equals, Var(n), Num(0)), Num(1), Binop(Times, Var(n), App(Var(fact), Binop(Minus, Var(n), Num(1)))))), App(Var(fact), Num(5)))")
    "exp_to_abstract_string Letrec complicated";
  unit_test (exp_to_abstract_string Raise = "Raise") "exp_to_abstract_string Raise";
  unit_test (exp_to_abstract_string Unassigned = "Unassigned") "exp_to_abstract_string Unassigned";
  unit_test (exp_to_abstract_string (App (Var "f", Var "x")) = "App(Var(f), Var(x))") "exp_to_abstract_string App";
  unit_test (exp_to_abstract_string (Float 3.14) = "Float(3.14)") "exp_to_abstract_string Float";
  unit_test (exp_to_abstract_string (Binop (FPlus, Float 2.5, Float 1.5)) = "Binop(FPlus, Float(2.5), Float(1.5))") "exp_to_abstract_string Binop FPlus";
  unit_test (exp_to_abstract_string (Binop (FMinus, Float 4., Float 1.5)) = "Binop(FMinus, Float(4.), Float(1.5))") "exp_to_abstract_string Binop FMinus";
  unit_test (exp_to_abstract_string (Binop (FTimes, Float 2., Float 3.)) = "Binop(FTimes, Float(2.), Float(3.))") "exp_to_abstract_string Binop FTimes";
  unit_test (exp_to_abstract_string (Binop (FDivide, Float 6., Float 2.)) = "Binop(FDivide, Float(6.), Float(2.))") "exp_to_abstract_string Binop FDivide";
;;

let free_vars_test () =
  print_endline "BEGINNING OF FREE_VARS_TEST!!" ;
(* I used same_vars to check if the identified free vars were equal to 
   the actual free vars *)
  unit_test (same_vars (free_vars (Var "x")) (vars_of_list ["x"]))
    "free_vars Var";
  unit_test (same_vars (free_vars (Unop (Negate, Var "x"))) (vars_of_list ["x"]))
    "free_vars Unop";
  unit_test (same_vars (free_vars (Binop (Plus, Var "x", Var "y"))) (vars_of_list ["x"; "y"]))
    "free_vars Binop";
  unit_test (same_vars (free_vars (Conditional (Var "x", Var "y", Var "z"))) (vars_of_list ["x"; "y"; "z"]))
    "free_vars Conditional";
  unit_test (same_vars (free_vars (Fun ("x", Binop (Plus, Var "x", Var "y")))) (vars_of_list ["y"]))
    "free_vars Fun";
  unit_test (same_vars (free_vars (Let ("x", Var "y", Binop (Plus, Var "x", Var "z")))) (vars_of_list ["y"; "z"]))
    "free_vars Let";
  unit_test (same_vars (free_vars (Letrec ("x", Binop (Plus, Var "x", Var "y"), Var "z"))) (vars_of_list ["y"; "z"]))
    "free_vars Letrec";
  unit_test (same_vars (free_vars (App (Var "x", Var "y"))) (vars_of_list ["x"; "y"]))
    "free_vars App";
;;

let subst_tests_both () =
  print_endline "BEGINNING OF SUBST_TEST!!" ;
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "x;;") = (str_to_exp "5;;")) "subst var with num";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "y;;") = (str_to_exp "y;;")) "subst var with different var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "10;;") = (str_to_exp "10;;")) "subst num with num";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "true;;") = (str_to_exp "true;;")) "subst bool with num";
  unit_test (subst "z" (Float 2.71) (Var "z") = Float 2.71) "subst var with float";
  unit_test ((subst "a" (str_to_exp "42;;") (str_to_exp "~- 5 - 3;;")) = (str_to_exp "~- 5 - 3;;")) "subst no match";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "~-x;;") = (str_to_exp "~-5;;")) "subst unop with var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "~-y;;") = (str_to_exp "~-y;;")) "subst unop with different var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "x + 3;;") = (str_to_exp "5 + 3;;")) "subst binop with var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "y + 3;;") = (str_to_exp "y + 3;;")) "subst binop with different var";
  unit_test ((subst "u" (Float 2.71) (Binop (FMinus, Var "u", Float 1.5))) = Binop (FMinus, Float 2.71, Float 1.5)) "subst binop with var and float";
  unit_test (subst "x" (str_to_exp "true;;") (str_to_exp "if x then 1 else 0;;") = (str_to_exp "if true then 1 else 0;;")) "subst conditional with var";
  unit_test (subst "x" (str_to_exp "true;;") (str_to_exp "if y then 1 else 0;;") = (str_to_exp "if y then 1 else 0;;")) "subst conditional with different var";
  unit_test ((subst "y" (str_to_exp "7;;") (str_to_exp "if y < 5 then x + y else ~- w;;")) = (str_to_exp "if 7 < 5 then x + 7 else ~- w;;")) "subst ifthenelse";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "fun x -> x;;") = (str_to_exp "fun x -> x;;")) "subst function with same var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "fun y -> x;;") = (str_to_exp "fun y -> 5;;")) "subst function with var";
  unit_test ((subst "z" (str_to_exp "x - 2;;") (str_to_exp "fun z -> z / 4;;")) = (str_to_exp "fun z -> z / 4;;")) "subst same var in fun";
  unit_test ((subst "y" (str_to_exp "x - 2;;") (str_to_exp "fun w -> w * y;;")) = (str_to_exp "fun w -> w * (x - 2);;")) "subst fun free var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "let x = 1 in x;;") = (str_to_exp "let x = 1 in x;;")) "subst let-";
  unit_test ((subst "a" (str_to_exp "b - 3;;") (str_to_exp "let a = 4 * a in a;;")) = (str_to_exp "let a = 4 * (b - 3) in a;;")) "subst let same var";
  unit_test ((subst "y" (str_to_exp "x - 2;;") (str_to_exp "let x = x + 2 * y in x - y;;")) = (str_to_exp "let var0 = var0 + 2 * (x - 2) in var0 - (x - 2);;")) "subst let free var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "let rec y = x in y;;") = (str_to_exp "let rec y = 5 in y;;")) "subst letrec with var";
  unit_test ((subst "b" (str_to_exp "a + 3;;") (str_to_exp "let rec b = 4 * b in b;;")) = (str_to_exp "let rec b = 4 * b in b;;")) "subst letrec same var";
  unit_test ((subst "x" (str_to_exp "y - 2;;") (str_to_exp "let rec y = y - 1 * x in y - x;;")) = (str_to_exp "let rec var1 = var1 - 1 * (y - 2) in var1 - (y - 2);;")) "subst letrec free var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "raise;;") = (str_to_exp "raise;;")) "subst raise";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "unassigned;;") = (str_to_exp "unassigned;;")) "subst unassignedd";
  unit_test (subst "x" (str_to_exp "fun y -> y + 1;;") (str_to_exp "x 3;;") = (str_to_exp "(fun y -> y + 1) 3;;")) "subst app with var";
  unit_test (subst "x" (str_to_exp "5;;") (str_to_exp "f x;;") = (str_to_exp "f 5;;")) "subst app with different var";
  unit_test ((subst "y" (str_to_exp "x + 3;;") (str_to_exp "(fun w -> y + w) y;;")) = (str_to_exp "(fun w -> (x + 3) + w) (x + 3);;")) "subst app";
  (* Tests for strings *)
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "x;;") = (str_to_exp "\"hello\";;")) "subst var with string";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "y;;") = (str_to_exp "y;;")) "subst var with different var (string)";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "\"world\";;") = (str_to_exp "\"world\";;")) "subst string with string";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "~-x;;") = (str_to_exp "~-\"hello\";;")) "subst unop with string var";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "x ^ \" world\";;") = (str_to_exp "\"hello\" ^ \" world\";;")) "subst binop with string var";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "if x = \"hello\" then 1 else 0;;") = (str_to_exp "if \"hello\" = \"hello\" then 1 else 0;;")) "subst conditional with string";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "fun x -> x;;") = (str_to_exp "fun x -> x;;")) "subst function with same string var";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "let x = \"world\" in x;;") = (str_to_exp "let x = \"world\" in x;;")) "subst let with string var";
  unit_test (subst "x" (str_to_exp "\"hello\";;") (str_to_exp "let rec y = x in y;;") = (str_to_exp "let rec y = \"hello\" in y;;")) "subst letrec with string var";
  unit_test (subst "x" (str_to_exp "fun y -> y ^ \"!\";;") (str_to_exp "x \"hello\";;") = (str_to_exp "(fun y -> y ^ \"!\") \"hello\";;")) "subst app with string var";
;;

(* Technically, the Env module methods are all tested through the evaluator tests below, 
   as these wouldn't pass if the Env methods were incorrect *)
(* I realized that a lot of my tests were applicable to all three models so I 
   abstracted them out into a test generator just like the eval_generator in 
   evaluation.ml*)
let model_test_generator (model : string) =
  let eval_test () =
    let eval_name = "eval_" ^ model in
    let eval_func = match model with
      | "s" -> eval_s
      | "d" -> eval_d
      | "l" -> eval_l
      | _ -> failwith "gen: invalid model"
    in
    let eval exp = let env = Env.empty () in eval_func exp env in
    print_endline ("BEGINNING OF " ^ eval_name ^ "TEST!!") ;
    (* !TEMPLATE UNIT TEST FOR ALL EVAL TESTS
       unit_test (eval (str_to_exp "_theexpression__;;") = Val (_____)) (eval_name ^ " _TESTNAME_");*)
    unit_test (eval (str_to_exp "42 ;;") = Val (Num 42)) (eval_name ^ " num");
    unit_test (eval (str_to_exp "true ;;") = Val (Bool true)) (eval_name ^ " bool");
    unit_test (try eval (str_to_exp "raise ;;") = Val (Num 0) with EvalException -> true | _ -> false) (eval_name ^ " raise");
    unit_test (try eval (str_to_exp "unassigned ;;") = Val (Num 0) with EvalError _ -> true | _ -> false) (eval_name ^ " unassigned");
    unit_test (try eval (str_to_exp "x ;;") = Val (Num 0) with EvalError _ -> true | _ -> false) (eval_name ^ " unbound var");
    unit_test (eval (str_to_exp "~-5 ;;") = Val (Num (-5))) (eval_name ^ " unop negate");
    unit_test (eval (str_to_exp "3 + 4 ;;") = Val (Num 7)) (eval_name ^ " binop plus");
    unit_test (eval (str_to_exp "5 - 2 ;;") = Val (Num 3)) (eval_name ^ " binop minus");
    unit_test (eval (str_to_exp "3 * 4 ;;") = Val (Num 12)) (eval_name ^ " binop times");
    unit_test (eval (str_to_exp "515 = 515 ;;") = Val (Bool true)) (eval_name ^ " binop equals");
    unit_test (eval (str_to_exp "0 < 2 ;;") = Val (Bool true)) (eval_name ^ " binop lessthan");
    unit_test (eval (str_to_exp "if true then 1 else 2 ;;") = Val (Num 1)) (eval_name ^ " if true");
    unit_test (eval (str_to_exp "if false then 1 else 2 ;;") = Val (Num 2)) (eval_name ^ " if false");
    unit_test (try eval (str_to_exp "if 1 then 1 else 2 ;;") = Val (Num 0) with EvalError _ -> true | _ -> false) (eval_name ^ " invalid if");
    unit_test (eval (str_to_exp "let x = 1 in x + 1 ;;") = Val (Num 2)) (eval_name ^ " let");
    unit_test (eval (str_to_exp "let x = 1 in let y = 2 in x + y ;;") = Val (Num 3)) (eval_name ^ " nested let");
    unit_test (eval (str_to_exp "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;") = Val (Num 24)) (eval_name ^ " letrec");
    unit_test (eval (str_to_exp "(fun x -> x + 1) 4 ;;") = Val (Num 5)) (eval_name ^ " app");
    unit_test (try eval (str_to_exp "1 2 ;;") = Val (Num 0) with EvalError _ -> true | _ -> false) (eval_name ^ " invalid app");
    unit_test (eval (Float 3.14) = Val (Float 3.14)) (eval_name ^ " float");
    unit_test (eval (Binop (FPlus, Float 2.5, Float 1.5)) = Val (Float 4.0)) (eval_name ^ " binop FPlus");
    unit_test (eval (Binop (FMinus, Float 4.0, Float 1.5)) = Val (Float 2.5)) (eval_name ^ " binop FMinus");
    unit_test (eval (Binop (FTimes, Float 2.0, Float 3.0)) = Val (Float 6.0)) (eval_name ^ " binop FTimes");
    unit_test (eval (Binop (FDivide, Float 6.0, Float 2.0)) = Val (Float 3.0)) (eval_name ^ " binop FDivide");
    unit_test (try eval (Binop (FDivide, Float 6.0, Float 0.0)) = Val (Float 0.0) with EvalError _ -> true | _ -> false) (eval_name ^ " binop FDivide by zero");
    (* String tests *)
    unit_test (eval (str_to_exp "\"hello\" ;;") = Val (String "hello")) (eval_name ^ " string literal");
    unit_test (eval (str_to_exp "\"hello\" + \" world\" ;;") = Val (String "hello world")) (eval_name ^ " string concatenation");
    unit_test (eval (str_to_exp "\"hello\" = \"hello\" ;;") = Val (Bool true)) (eval_name ^ " string equality");
    unit_test (eval (str_to_exp "\"hello\" = \"wor`ld\" ;;") = Val (Bool false)) (eval_name ^ " string inequality");
    unit_test (eval (str_to_exp "\"abc\" < \"def\" ;;") = Val (Bool true)) (eval_name ^ " string less than");
    unit_test (eval (str_to_exp "\"def\" < \"abc\" ;;") = Val (Bool false)) (eval_name ^ " string greater than");
    unit_test (eval (str_to_exp "let x = \"hello\" in x + \" world\" ;;") = Val (String "hello world")) (eval_name ^ " string let binding");
    unit_test (eval (str_to_exp "let greet = fun name -> \"Hello, \" + name in greet \"Alice\" ;;") = Val (String "Hello, Alice")) (eval_name ^ " string function application");

    (* Specific tests for each model *)
    match model with
    | "s" ->
      unit_test (eval (str_to_exp "fun x -> x + 1 ;;") = Val (Fun ("x", Binop (Plus, Var "x", Num 1)))) (eval_name ^ " fun");
      unit_test (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") = Val (Num 4)) (eval_name ^ " s scope");
      unit_test (eval (str_to_exp "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5 ;;") = Val (Num 120)) (eval_name ^ " recursive binding");
      unit_test (eval (str_to_exp "let twice = fun f -> fun x -> f (f x) in let inc = fun x -> x + 1 in twice inc 3 ;;") = Val (Num 5)) (eval_name ^ " higher-order functions");
      unit_test (eval (str_to_exp "let x = 1 in let x = 2 in x ;;") = Val (Num 2)) (eval_name ^ " shadowing vars");
    | "d" ->
      unit_test (eval (str_to_exp "fun x -> x + 1 ;;") = Val (Fun ("x", Binop (Plus, Var "x", Num 1)))) (eval_name ^ " fun");
      unit_test (eval (str_to_exp "let x = 1 in let y = 2 in let z = x + y in z * z ;;") = Val (Num 9)) (eval_name ^ " let scope");
      unit_test (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") = Val (Num 5)) (eval_name ^ " dynamic scope");
      unit_test (eval (str_to_exp "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5 ;;") = Val (Num 120)) (eval_name ^ " recursive binding");
      unit_test (eval (str_to_exp "let x = 1 in let x = 2 in x ;;") = Val (Num 2)) (eval_name ^ " shadowing vars");
      unit_test (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f x ;;") = Val (Num 4)) (eval_name ^ " capturing vars");
      unit_test (eval (str_to_exp "let x = 1 in let rec f n = if n = 0 then x else let x = 2 in f (n - 1) in f 5 ;;") = Val (Num 2)) (eval_name ^ " shadowing in letrec");
    | "l" ->
      unit_test (eval (str_to_exp "let x = 1 in let y = 2 in let z = x + y in z * z ;;") = Val (Num 9)) (eval_name ^ " let scope");
      unit_test (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") = Val (Num 4)) (eval_name ^ " lexical scope");
      unit_test (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f x ;;") = Val (Num 3)) (eval_name ^ " lexical scope closure");
      unit_test (eval (str_to_exp "let f = let x = 1 in fun y -> x + y in let x = 2 in f 3 ;;") = Val (Num 4)) (eval_name ^ " closure environmentt");
      unit_test (eval (str_to_exp "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5 ;;") = Val (Num 120)) (eval_name ^ " recursive binding");
      unit_test (eval (str_to_exp "let twice = fun f -> fun x -> f (f x) in let inc = fun x -> x + 1 in twice inc 3 ;;") = Val (Num 5)) (eval_name ^ " higher-order functions");
      unit_test (eval (str_to_exp "let x = 1 in let x = 2 in x ;;") = Val (Num 2)) (eval_name ^ " shadowing vars");
      unit_test (eval (str_to_exp "let x = 1 in let rec f n = if n = 0 then x else let x = 2 in f (n - 1) in f 5 ;;") = Val (Num 1)) (eval_name ^ " shadowing in letrec");
      (* Currying tests after I implemented it *)
      unit_test (eval (str_to_exp "let add x y = x + y in add 1 2 ;;") = Val (Num 3)) (eval_name ^ " currying basic");
      unit_test (eval (str_to_exp "let add x y = x + y in let add1 = add 1 in add1 2 ;;") = Val (Num 3)) (eval_name ^ " currying partial");
      unit_test (eval (str_to_exp "let add x y = x + y in let add1 = add 1 in let add2 = add 2 in add1 3 + add2 4 ;;") = Val (Num 10)) (eval_name ^ " currying multiple");
      unit_test (eval (str_to_exp "let add x y = x + y in let addx x = add x in addx 1 2 ;;") = Val (Num 3)) (eval_name ^ " currying higher-order");
      unit_test (eval (str_to_exp "let add x y = x + y in let apply f x y = f x y in apply add 1 2 ;;") = Val (Num 3)) (eval_name ^ " currying apply");
      unit_test (eval (str_to_exp "let add x y z = x + y + z in add 1 2 3 ;;") = Val (Num 6)) (eval_name ^ " currying multiple arguments");
      unit_test (eval (str_to_exp "let add x y = x +. y in add 1.5 2.5 ;;") = Val (Float 4.0)) (eval_name ^ " currying with floats");
      (* additional string tests *)
      (* for scoping *)
      unit_test (eval (str_to_exp "let x = \"outer\" in let f y = x + y in let x = \"inner\" in f \" scope\" ;;") = Val (String "outer scope")) (eval_name ^ " string lexical scope");
      unit_test (eval (str_to_exp "let x = \"global\" in let f = let x = \"local\" in fun y -> x + y in f \" scope\" ;;") = Val (String "local scope")) (eval_name ^ " string closure environment");
    | _ -> ()
  in eval_test ()

let test_all () =
  exp_to_concrete_string_test () ;
  exp_to_abstract_string_test () ;
  free_vars_test () ;
  subst_tests_both () ;
  model_test_generator "s" ;
  model_test_generator "d" ;
  model_test_generator "l" ;
;;

let _ = test_all () ;;
