open Types

(* prettyprinting util *)

let rec ty_to_string (t : ty) : string =
    begin match t with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TList t2 -> "[" ^ (ty_to_string t2) ^ "]"
    | TFunc (t1, t2) -> "(" ^ (ty_to_string t1) ^ " -> " ^ (ty_to_string t2) ^ ")"
    | TFuncMulti (args, ret) -> "(" ^ (List.fold_right (fun t s -> ty_to_string t ^ " -> " ^ s) args "") ^ ty_to_string ret ^ ")"
    | TPoly s -> s
    | TFuncExt (delta, ret) -> (List.fold_right (fun (_,t) s -> ty_to_string t ^ " -> " ^ s) !delta "") ^ ty_to_string ret
    | TMaybe s -> "Maybe " ^ ty_to_string s
    | TTuple (t1, t2) -> "(" ^ (ty_to_string t1) ^ "," ^ (ty_to_string t2) ^ ")"
    end

let rec ty_to_string_ocaml (t : ty) : string =
    begin match t with
    | TInt -> "int"
    | TBool -> "bool"
    | TList t2 -> "(" ^ (ty_to_string_ocaml t2) ^ ") list"
    | TFunc (t1, t2) -> "(" ^ (ty_to_string_ocaml t1) ^ " -> " ^ (ty_to_string_ocaml t2) ^ ")"
    | TFuncMulti (args, ret) -> "(" ^ (List.fold_right (fun t s -> ty_to_string_ocaml t ^ " -> " ^ s) args "") ^ ty_to_string_ocaml ret ^ ")"
    | TPoly s -> s
    | TFuncExt (delta, ret) -> (List.fold_right (fun (_,t) s -> ty_to_string_ocaml t ^ " -> " ^ s) !delta "") ^ ty_to_string_ocaml ret
    | TMaybe s -> "(" ^ ty_to_string_ocaml s ^ ") option"
    | TTuple (t1, t2) -> "(" ^ (ty_to_string_ocaml t1) ^ " * " ^ (ty_to_string_ocaml t2) ^ ")"
    end

let haskell_to_ocaml (s : string) : string =
    begin match s with
    | "(:)" -> "cons"
    | "len" -> "List.length"
    | "True" -> "true"
    | "False" -> "false"
    | "negate" -> "(~-)"
    | "id" -> "(fun id -> id)"
    | "map" -> "List.map"
    | "filter" -> "List.filter"
    | "(.)" -> "compose"
    | "(Main.<)" -> "(<)"
    | "(Main.==)" -> "(=)"
    | "(++)" -> "(@)"
    | s -> s
    end

(* PRETTYPRINTING *)

let rec prettyprint_haskell (e : expr) : string = 
    begin match e with
    | EVar (s, _, _) -> if String.starts_with ~prefix:"EVIL" s then String.sub s 4 ((String.length s)-4) else s 
    | ELam (e1, e2) -> 
        let context = ref [] in
        begin match e1 with
            | EVar (_, ct, _) -> context := ct;
            | _ -> failwith "argument of a lambda is not an EVar"
        end;
        let argumentType = lookup e1 !context in
        if (usesArgument e1 e2) || is_polymorphic argumentType then ("(\\" ^ (prettyprint_haskell e1) ^ " -> " ^ (prettyprint_haskell e2) ^ ")") else
        "(\\" ^ (prettyprint_haskell e1) ^ " -> let _ = (" ^ (prettyprint_haskell e1) ^ " :: " ^ (ty_to_string argumentType) ^ ") in " ^ (prettyprint_haskell e2) ^ ")"
    | ELamMulti (delta, body) ->
        let arguments = ref "" in
        for i=0 to (List.length delta)-1 do
            let (arg_expr, arg_type) = List.nth delta i in
            arguments := !arguments ^ " " ^ (prettyprint_haskell arg_expr);
        done;
        "(\\" ^ !arguments ^ " -> " ^ (prettyprint_haskell body) ^ ")"
    | EApp (e1, e2) -> "(" ^ (prettyprint_haskell e1) ^ ") (" ^ (prettyprint_haskell e2) ^ ")" 
    | EAppMulti (e1, args) ->
        let res = ref "" in
        let sugarIfThenElse = ref false in
        begin match e1 with
        | EVar (s, _, _) ->
            if String.ends_with ~suffix:"ifthenelse" s then (
                sugarIfThenElse := true;
                let condition = List.nth args 0 in
                let thenBranch = List.nth args 1 in
                let elseBranch = List.nth args 2 in
                res := "(if (" ^ (prettyprint_haskell condition) ^ ") then (" ^ (prettyprint_haskell thenBranch) ^ ") else (" ^ (prettyprint_haskell elseBranch) ^ "))"
            ) else (
                res := ("(" ^ (prettyprint_haskell e1) ^ ") ")
            )
        | ELamMulti _ -> res := ("(" ^ (prettyprint_haskell e1) ^ ") ");
        | _ -> failwith "tried applying something other than a lambda or variable"
        end;
        if !sugarIfThenElse then !res else
        (for i = 0 to (List.length args) - 1 do
            let arg_expr = List.nth args i in
            res := !res ^ "(" ^ (prettyprint_haskell arg_expr) ^ ") "
        done;
        !res)
    | ELet (freshvar, e1, e2) -> "let " ^ (prettyprint_haskell freshvar) ^ " = (" ^ (prettyprint_haskell e1) ^ ") in (" ^ (prettyprint_haskell e2) ^ ")" 
    | EPatternMatch (list, base, step, x, xs) -> 
        "(case " ^ (prettyprint_haskell list) ^ " of [] -> " ^ (prettyprint_haskell base) ^ "; (" ^ (prettyprint_haskell x) ^ ":" ^ (prettyprint_haskell xs) ^ ") -> " ^ (prettyprint_haskell step) ^ ";)"
    | ETuple (e1, e2) -> "(" ^ (prettyprint_haskell e1) ^ "," ^ (prettyprint_haskell e2) ^ ")"
    | EMaybe (m, n) -> "(case " ^ (prettyprint_haskell m) ^ " of Nothing -> " ^ (prettyprint_haskell n) ^ "; Just x -> x;)"
    end

let rec prettyprint_ocaml (e : expr) : string = 
    begin match e with
    | EVar (s, _, _) -> if String.starts_with ~prefix:"EVIL" s then haskell_to_ocaml (String.sub s 4 ((String.length s)-4)) else haskell_to_ocaml s 
    | ELam (e1, e2) ->
        let context = ref [] in
        begin match e1 with
            | EVar (_, ct, _) -> context := ct;
            | _ -> failwith "argument of a lambda is not an EVar"
        end;
        let argumentType = lookup e1 !context in
        if (usesArgument e1 e2) || is_polymorphic argumentType then ("(fun " ^ (prettyprint_ocaml e1) ^ " -> " ^ (prettyprint_ocaml e2) ^ ")") else
        "(fun (" ^ (prettyprint_ocaml e1) ^ " : " ^ (ty_to_string_ocaml argumentType) ^ ") -> " ^ (prettyprint_ocaml e2) ^ ")"
    | ELamMulti (delta, body) ->
        let arguments = ref "" in
        for i=0 to (List.length delta)-1 do
            let (arg_expr, arg_type) = List.nth delta i in
            arguments := !arguments ^ " " ^ (prettyprint_ocaml arg_expr);
        done;
        "(fun " ^ !arguments ^ " -> " ^ (prettyprint_ocaml body) ^ ")"
    | EApp (e1, e2) -> "(" ^ (prettyprint_ocaml e1) ^ ") (" ^ (prettyprint_ocaml e2) ^ ")" 
    | EAppMulti (e1, args) ->
        let res = ref "" in
        let sugarIfThenElse = ref false in
        begin match e1 with
        | EVar (s, _, _) ->
            if String.ends_with ~suffix:"ifthenelse" s then (
                sugarIfThenElse := true;
                let condition = List.nth args 0 in
                let thenBranch = List.nth args 1 in
                let elseBranch = List.nth args 2 in
                res := "(if (" ^ (prettyprint_ocaml condition) ^ ") then (" ^ (prettyprint_ocaml thenBranch) ^ ") else (" ^ (prettyprint_ocaml elseBranch) ^ "))"
            ) else (
                res := ("(" ^ (prettyprint_ocaml e1) ^ ") ")
            )
        | ELamMulti _ -> res := ("(" ^ (prettyprint_ocaml e1) ^ ") ");
        | _ -> failwith "tried applying something other than a lambda or variable"
        end;
        if !sugarIfThenElse then !res else
        (for i = 0 to (List.length args) - 1 do
            let arg_expr = List.nth args i in
            res := !res ^ "(" ^ (prettyprint_ocaml arg_expr) ^ ") "
        done;
        !res)
    | ELet (freshvar, e1, e2) -> "let " ^ (prettyprint_ocaml freshvar) ^ " = (" ^ (prettyprint_ocaml e1) ^ ") in (" ^ (prettyprint_ocaml e2) ^ ")" 
    | EPatternMatch (list, base, step, x, xs) -> 
        "(begin match " ^ (prettyprint_ocaml list) ^ " with | [] -> " ^ (prettyprint_ocaml base) ^ "; | (" ^ (prettyprint_ocaml x) ^ "::" ^ (prettyprint_ocaml xs) ^ ") -> " ^ (prettyprint_ocaml step) ^ " end)"
    | ETuple (e1, e2) -> "(" ^ (prettyprint_ocaml e1) ^ "," ^ (prettyprint_ocaml e2) ^ ")"
    | EMaybe (m, n) -> "(begin match " ^ (prettyprint_ocaml m) ^ " with | None -> " ^ (prettyprint_ocaml n) ^ "; | Some x -> x end)"
    end

(* PROGRAM PREFIX *)

let program_prefix_haskell : string =
"import Distribution.Simple.Utils

main = putStrLn \"executed main\"

len :: [a] -> Int
len = foldr (\\_ n -> n+1) 0

(<) :: Int -> Int -> Bool
(<) = (Prelude.<)

(==) :: Bool -> Bool -> Bool
(==) = (Prelude.==)

ifthenelse :: Bool -> a -> a -> a
ifthenelse c a b = if c then a else b

welltypedProgram"

let program_prefix_ocaml : string =
"let safeHead list = 
    match list with
    | [] -> None
    | (h::t) -> Some h

let safeTail list =
    match list with
    | [] -> []
    | (h::t) -> t

let compose f g x = f (g x)

let ifthenelse c a b = if c then a else b

let cons x xs = x :: xs

let welltypedProgram"