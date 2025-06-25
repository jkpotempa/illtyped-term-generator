open Helpers

type ty = TInt 
        | TBool 
        | TList of ty 
        | TFunc of ty * ty 
        | TFuncMulti of (ty list * ty) 
        | TPoly of string
        | TFuncExt of (ctxt ref * ty)
        | TMaybe of ty
        | TTuple of ty * ty
and expr =    EVar of (string * ctxt) (* storing context in Var rules is necessary, as it is needed for evil rule introduction *)
            | ELam of (expr * expr) 
            | ELamMulti of (ctxt * expr)
            | EApp of (expr * expr) 
            | EAppMulti of (expr * expr list)
            | ELet of (expr * expr * expr)
            | EPatternMatch of (expr * expr * expr * expr * expr)
            | ETuple of (expr * expr)
            | EMaybe of (expr * expr)
and  ctxt = (expr * ty) list

type rule = Var | Lam | App | Indir | LamExt | AppExt | ParamExt | Let | PatternMatch | Maybe | Tuple

type constraints = (ty * ty) list

type ty_hashtbl = (ty, ty) Hashtbl.t

let hashtable_to_list (ht : ty_hashtbl) : constraints =
    Hashtbl.fold (fun k v ls -> (k, v) :: ls) ht []

let rule_to_string (r : rule) =
    begin match r with
    | Var -> "Var" 
    | Lam -> "Lam"
    | App -> "App"
    | Indir -> "Indir"
    | LamExt -> "LamExt"
    | AppExt -> "AppExt"
    | ParamExt -> "ParamExt"
    | Let -> "Let"
    | PatternMatch -> "PatternMatch"
    | Maybe -> "Maybe"
    | Tuple -> "Tuple"
    end

let rec log_ty (t : ty) = 
    begin match t with
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TList t -> "TList (" ^ (log_ty t) ^ ")" 
    | TFunc (arg, ret) -> "TFunc (" ^ (log_ty arg) ^ ", " ^ (log_ty ret) ^ ")" 
    | TFuncMulti (args, ret) -> "TFuncMulti (" ^ (log_ty_list args) ^ ", " ^ (log_ty ret) ^ ")" 
    | TPoly s -> "TPoly \"" ^ s ^ "\""
    | TFuncExt (delta, ret) -> "TFuncExt (ref " ^ (log_ctxt !delta) ^ ", " ^ (log_ty ret) ^ ")"
    | TMaybe s -> "TMaybe (" ^ (log_ty s) ^ ")"
    | TTuple (t1, t2) -> "TTuple (" ^ (log_ty t1) ^ ", " ^ (log_ty t2) ^ ")" 
    end
and log_ty_list (ts : ty list) = 
    let res = ref "[" in
    for i = 0 to (List.length ts)-1 do
        res := !res ^ (log_ty (List.nth ts i)) ^ ";"
    done;
    !res ^ "]"
and log_ctxt (cs : ctxt) =
    let res = ref "[" in
    for i = 0 to (List.length cs)-1 do
        let curr = List.nth cs i in
        res := !res ^ "(" ^ (log_expr (fst curr)) ^ ", " ^ (log_ty (snd curr)) ^ ");"
    done;
    !res ^ "]"
and log_expr (e : expr) = 
    begin match e with 
    | EVar (s, cs) -> "EVar (\"" ^ s ^ "\", " ^ (log_ctxt cs) ^ ")"
    | ELam (e1, e2) -> "ELam (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ")"
    | ELamMulti (cs, body) -> "ELamMulti (" ^ (log_ctxt cs) ^ ", " ^ (log_expr body) ^ ")"
    | EApp (e1, e2) -> "EApp (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ")" 
    | EAppMulti (e1, es) -> "EAppMulti (" ^ (log_expr e1) ^ ", " ^ (log_expr_list es) ^ ")"
    | ELet (e1, e2, e3) -> "ELet (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ", " ^ (log_expr e3) ^ ")" 
    | EPatternMatch (e1, e2, e3, e4, e5) -> "EPatternMatch (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ", " ^ (log_expr e3) ^ ", " ^ (log_expr e4) ^ ", " ^ (log_expr e5) ^ ")" 
    | ETuple (e1, e2) -> "ETuple (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ")"
    | EMaybe (e1, e2) -> "EMaybe (" ^ (log_expr e1) ^ ", " ^ (log_expr e2) ^ ")"
    end
and log_expr_list (es : expr list) = 
    let res = ref "[" in
    for i = 0 to (List.length es)-1 do
        res := !res ^ (log_expr (List.nth es i)) ^ ";"
    done;
    !res ^ "]"
and log_ty_hashtbl (ht : ty_hashtbl) =
    let res = ref "[" in
    let list = hashtable_to_list ht in
    for i = 0 to (List.length list)-1 do
        let curr = List.nth list i in
        res := !res ^ (log_ty (fst curr)) ^ "," ^ (log_ty (snd curr)) ^ "; "
    done;
    !res ^ "]"

(* REPLACED BY UNIFY *)
(* outputs true if t1 <= t2 and false otherwise. *)
(* let rec subtype (t1 : ty) (t2 : ty) : bool = 
    if ty_to_string t1 = ty_to_string t2 then true else
    begin match t1 with
    | TInt ->
        begin match t2 with
        | TPoly _ -> true
        | TInt -> true
        | _ -> false
        end
    | TBool ->
        begin match t2 with
        | TPoly _ -> true
        | TBool -> true
        | _ -> false
        end
    | TList elem_ty1 ->
        begin match t2 with
        | TPoly _ -> true
        | TList elem_ty2 -> subtype elem_ty1 elem_ty2
        | _ -> false
        end
    | TFunc (arg1, ret1) ->
        begin match t2 with
        | TPoly _ -> true
        | TFunc (arg2, ret2) | TFuncMulti (arg2::[], ret2) -> (subtype arg2 arg1) && (subtype ret1 ret2)
        | TFuncExt (delta2, ret2) -> if (List.length !delta2) <> 1 then false else (subtype (snd (List.hd !delta2)) arg1) && (subtype ret1 ret2)
        | _ -> false
        end
    | TFuncMulti (args1, ret1) ->
        begin match t2 with
        | TPoly _ -> true
        | TFunc (arg2, ret2) -> if (List.length args1) = 1 then (subtype arg2 (List.hd args1)) && (subtype ret1 ret2) else false
        | TFuncMulti (args2, ret2) -> if (List.length args1) <> (List.length args2) then false else
            let arg_pairs = List.combine args1 args2 in
            let args_ok : bool = List.for_all Fun.id (List.map (fun (a, b) -> subtype b a) arg_pairs) in
            args_ok && (subtype ret1 ret2)
        | TFuncExt (delta2, ret2) -> 
            let args2 = List.map snd !delta2 in
            let arg_pairs = List.combine args1 args2 in
            let args_ok : bool = List.for_all Fun.id (List.map (fun (a, b) -> subtype b a) arg_pairs) in
            args_ok && (subtype ret1 ret2)
        | _ -> false
        end
    | TPoly _ ->
        begin match t2 with
        | TPoly _ -> true
        | _ -> false
        end
    | TFuncExt (delta1, ret1) ->
        let args1 = List.map snd !delta1 in
        begin match t2 with
        | TPoly _ -> true
        | TFunc (arg2, ret2) -> if (List.length args1) = 1 then (subtype arg2 (List.hd args1)) && (subtype ret1 ret2) else false
        | TFuncMulti (args2, ret2) -> if (List.length args1) <> (List.length args2) then false else
            let arg_pairs = List.combine args1 args2 in
            let args_ok : bool = List.for_all Fun.id (List.map (fun (a, b) -> subtype b a) arg_pairs) in
            args_ok && (subtype ret1 ret2)
        | TFuncExt (delta2, ret2) -> 
            let args2 = List.map snd !delta2 in
            let arg_pairs = List.combine args1 args2 in
            let args_ok : bool = List.for_all Fun.id (List.map (fun (a, b) -> subtype b a) arg_pairs) in
            args_ok && (subtype ret1 ret2)
        | _ -> false
        end
    end *)

let rec right_associate (t : ty) : ty = 
    begin match t with
    | TFuncMulti (args, ret) -> 
        let n = List.length args in
        begin match n with
        | 0 -> t
        | 1 -> TFunc (List.hd args, ret)
        | n -> right_associate (TFuncMulti (take (n-1) args, TFunc (List.nth args (n-1), ret)))
        end
    | TFuncExt (delta, ret) ->
        let args = List.map snd !delta in
        right_associate (TFuncMulti (args, ret))
    | _ -> t
    end    

let rec lookup_ty (t : ty) (ht : ty_hashtbl) (seen : ty list) : ty =
    try
        if contains t seen then t(*print_endline ("cycle of length " ^ (string_of_int (List.length seen)) ^ " detected"); t*) else
        (begin match t with
        | TInt | TBool -> t 
        | TList s -> TList (lookup_ty s ht (t :: seen))
        | TFunc (s1, s2) -> TFunc (lookup_ty s1 ht (t :: seen), lookup_ty s2 ht (t :: seen))
        | TFuncMulti (args, ret) -> TFuncMulti (List.map (fun a -> lookup_ty a ht (t :: seen)) args, lookup_ty ret ht (t :: seen)) 
        | TFuncExt (delta, ret) ->
            let olddelta = !delta in
            delta := [];
            for i = 0 to (List.length olddelta)-1 do
                let e, a = List.nth olddelta i in
                delta := !delta @ [e, lookup_ty a ht (t :: seen)] 
            done;
            TFuncExt (delta, lookup_ty ret ht (t :: seen))
        | TPoly s -> 
            let res = Hashtbl.find_opt ht t in
            begin match res with
            | None -> t
            | Some substitution -> 
                let looked_up = lookup_ty substitution ht (t :: seen) in
                begin match looked_up with
                | TPoly _ -> (* uninstantiated *) t
                | _ -> (* instantiated *) looked_up
                end
            end
        | TMaybe s -> TMaybe (lookup_ty s ht (t :: seen))
        | TTuple (s1, s2) -> TTuple (lookup_ty s1 ht (t :: seen), lookup_ty s2 ht (t :: seen))
        end)
    with Not_found -> t

let rec occurs_free_in (s : ty) (t : ty) : bool =
    match t with
    | TInt | TBool -> false
    | TPoly x -> 
        begin match s with
        | TPoly w -> x = w
        | _ -> false
        end
    | TFunc (t1, t2) -> (occurs_free_in s t1) || (occurs_free_in s t2)
    | TList t1 -> occurs_free_in s t1
    | TFuncMulti (args, ret) -> (List.exists (occurs_free_in s) args) || occurs_free_in s ret
    | TFuncExt (delta, ret) -> (List.exists (fun (_, arg_type) -> occurs_free_in s arg_type) !delta) || occurs_free_in s ret
    | TMaybe t1 -> occurs_free_in s t1
    | TTuple (t1, t2) -> (occurs_free_in s t1) || (occurs_free_in s t2)

(* substitutes s with t in the type constr *)
let rec substitute (s : ty) (t : ty) (constr : ty) : ty =
    begin match constr with 
    | TInt | TBool | TPoly _ -> if s = constr then t else constr
    | TList w -> TList (substitute s t w)
    | TFunc (w1, w2) -> TFunc (substitute s t w1, substitute s t w2)
    | TFuncMulti (args, ret) -> TFuncMulti (List.map (substitute s t) args, substitute s t ret)
    | TFuncExt (delta, ret) -> if List.length !delta = 0 then constr else 
        let newdelta = List.map (fun (e, arg_type) -> e, substitute s t arg_type) !delta in
        TFuncExt (ref newdelta, ret)
    | TMaybe w -> TMaybe (substitute s t w)
    | TTuple (w1, w2) -> TTuple (substitute s t w1, substitute s t w2)
    end

let rec unify (cs : constraints option) (substitutions : ty_hashtbl) : (constraints * ty_hashtbl) option =
    begin match cs with
    | None -> None
    | Some [] -> Some ([], substitutions)
    | Some ((s, t) :: cs') ->
        (* DEBUG *)
        (* print_endline ("s: " ^ (ty_to_string s));
        print_endline ("t: " ^ (ty_to_string t));
        print_endline "cs': "; 
        for i=0 to (List.length cs' )-1 do
            let elem = List.nth cs' i in
            print_endline ((ty_to_string (fst elem)) ^ " " ^ (ty_to_string (snd elem)));
        done;
        print_endline ("substitutions: " ^ (log_ty_hashtbl substitutions)); *)
        (* END DEBUG *)
        let s = lookup_ty s substitutions [] in
        let t = lookup_ty t substitutions [] in
        if s = t then unify (Some cs') substitutions
        else
        begin match s, t with
        | TPoly x, _ -> if not (occurs_free_in s t) then 
            begin match Hashtbl.find_opt substitutions s with
            | None ->
                 (Hashtbl.replace substitutions s t; unify (Some cs') substitutions)
            | Some currentBinding -> (*print_endline "THERE IS A CURRENT BINDING!";*) Hashtbl.replace substitutions s t; Hashtbl.replace substitutions t currentBinding; unify (Some cs') substitutions
            end
            else None
        | _, TPoly x -> if not (occurs_free_in t s) then 
            begin match Hashtbl.find_opt substitutions t with
            | None ->
                 (Hashtbl.replace substitutions t s; unify (Some cs') substitutions)
            | Some currentBinding -> (*print_endline "THERE IS A CURRENT BINDING!";*) Hashtbl.replace substitutions t s; Hashtbl.replace substitutions s currentBinding; unify (Some cs') substitutions
            end
            else None
        | TFunc (s1, s2), TFunc (u1, u2) -> unify (Some (cs' @ [(s1, u1); (s2, u2)])) substitutions
        | TFunc _, TFuncMulti _ | TFuncMulti _, TFunc _ | TFuncMulti _, TFuncMulti _ ->
            unify (Some ([(right_associate s, right_associate t)] @ cs')) substitutions
        | TFunc _, TFuncExt (delta, _) | TFuncMulti _, TFuncExt (delta, _) | TFuncExt (delta, _), TFunc _ | TFuncExt (delta, _), TFuncMulti _ ->
            if List.length !delta = 0 then None else unify (Some ([(right_associate s, right_associate t)] @ cs')) substitutions
        | TFuncExt (delta1, ret1), TFuncExt (delta2, ret2) -> 
            if List.length !delta1 = 0 || List.length !delta2 = 0 then None else unify (Some ([(right_associate s, right_associate t)] @ cs')) substitutions
        | TList w1, TList w2 -> unify (Some (cs' @ [w1, w2])) substitutions
        | TMaybe w1, TMaybe w2 -> unify (Some (cs' @ [w1, w2])) substitutions
        | TTuple (s1, s2), TTuple (u1, u2) -> unify (Some (cs' @ [(s1, u1); (s2, u2)])) substitutions
        | _ -> None
        end
    end

let rec is_polymorphic (t : ty) : bool =
    match t with
    | TInt -> false
    | TBool -> false
    | TList s -> is_polymorphic s 
    | TFunc (arg, ret) -> (is_polymorphic arg) || (is_polymorphic arg)
    | TFuncMulti (args, ret) -> (List.exists (is_polymorphic) args) || is_polymorphic ret
    | TFuncExt (delta, ret) -> (List.exists (fun (_, arg_ty) -> is_polymorphic arg_ty) !delta) || (is_polymorphic ret)
    | TPoly _ -> true
    | TMaybe s -> is_polymorphic s
    | TTuple (s1, s2) -> (is_polymorphic s1) || (is_polymorphic s2)

let basic_types = [TInt; TBool; TList TInt; TList TBool] (* arbitrary design choice *)

let gamma : ctxt =  [(EVar ("(+)", []), TFuncMulti ([TInt; TInt], TInt))
                    ;(EVar ("(-)", []), TFuncMulti ([TInt; TInt], TInt))
                    ;(EVar ("(||)", []), TFuncMulti ([TBool; TBool], TBool))
                    ;(EVar ("(&&)", []), TFuncMulti ([TBool; TBool], TBool))
                    ;(EVar ("len", []), TFuncMulti ([TList (TPoly "a")], TInt))
                    ;(EVar ("(:)", []), TFuncMulti ([TPoly "a"; TList (TPoly "a")], TList (TPoly "a")))
                    ;(EVar ("0", []), TInt)
                    ;(EVar ("1", []), TInt)
                    ;(EVar ("True", []), TBool)
                    ;(EVar ("False", []), TBool)
                    ;(EVar ("[]", []), TList (TPoly "a"))
                    ;(EVar ("negate", []), TFuncMulti ([TInt], TInt))
                    ;(EVar ("negate", []), TFunc (TInt, TInt))
                    ;(EVar ("(Main.<)", []), TFuncMulti ([TInt; TInt], TBool))
                    ;(EVar ("(Main.==)", []), TFuncMulti ([TBool; TBool], TBool))
                    ;(EVar ("map", []), TFuncMulti ([TFunc (TPoly "a", TPoly "b"); TList (TPoly "a")], TList (TPoly "b")))
                    ;(EVar ("filter", []), TFuncMulti ([TFunc (TPoly "a", TBool); TList (TPoly "a")], TList (TPoly "a")))
                    ;(EVar ("safeHead", []), TFuncMulti ([TList (TPoly "a")], TMaybe (TPoly "a")))
                    ;(EVar ("safeTail", []), TFuncMulti ([TList (TPoly "a")], TList (TPoly "a")))
                    ;(EVar ("id", []), TFuncMulti ([TPoly "a"], TPoly "a"))
                    ;(EVar ("id", []), TFunc (TPoly "a", TPoly "a"))
                    ;(EVar ("(++)", []), TFuncMulti ([TList (TPoly "a"); TList (TPoly "a")], TList (TPoly "a")))
                    ;(EVar ("ifthenelse", []), TFuncMulti ([TBool; TPoly "a"; TPoly "a"], TPoly "a"))
                    ;(EVar ("fst", []), TFuncMulti ([TTuple (TPoly "a", TPoly "b")], TPoly "a"))
                    ;(EVar ("snd", []), TFuncMulti ([TTuple (TPoly "a", TPoly "b")], TPoly "b"))
                    ;(EVar ("(.)", []), TFuncMulti ([TFunc (TPoly "b", TPoly "c"); TFunc (TPoly "a", TPoly "b"); TPoly "a"], TPoly "c"))                    
                    (* ;(EVar ("lam1", []), TFuncMulti ([List.hd (shuffle basic_types)], List.hd (shuffle basic_types)))
                    ;(EVar ("lam2", []), TFuncMulti ([List.hd (shuffle basic_types)], List.hd (shuffle basic_types)))
                    ;(EVar ("lam3", []), TFuncMulti ([List.hd (shuffle basic_types)], List.hd (shuffle basic_types))) *)
                    ]

let rec lookup (e : expr) (ct : ctxt) : ty =
    let ct = ct @ gamma in
    begin match e with
        | EVar (s, _) -> try 
            snd (List.find (fun (elem, _) -> elem = EVar (s, [])) ct) (* CAREFUL: finds only the first element. watch out for potential TFunc / TFuncMulti conflicts*)
            with Not_found -> failwith ("NOT FOUND. tried looking for " ^ (log_expr e) ^ " in " ^ (log_ctxt ct))
        | _ -> failwith "tried to look up non variable inside context"
    end

(* checks if the argument of a lambda (e1) is used by its body (e2) *)
let rec usesArgument (e1 : expr) (e2 : expr) : bool =
    let argName = ref "" in
    begin match e1 with
        | EVar (s, _) -> argName := s;
        | _ -> failwith "argument of a lambda is not an EVar"
    end;
    begin match e2 with
        | EVar (s, _) -> !argName = s
        | ELam (_, e3) | ELamMulti (_, e3) -> usesArgument e1 e3
        | EApp (e3, e4) | ELet (_, e3, e4) | ETuple (e3, e4) | EMaybe (e3, e4) -> (usesArgument e1 e3) || (usesArgument e1 e4) 
        | EAppMulti (e3, args) -> (usesArgument e1 e3) || List.fold_right (fun e b -> (usesArgument e1 e) || b) args false
        | EPatternMatch (list, base, step, _, _) -> (usesArgument e1 list) || (usesArgument e1 base) || (usesArgument e1 step)
    end

let rec evil_rule_in_subtree (e : expr) : bool =
    begin match e with
    | EVar (s, _) -> String.starts_with ~prefix:"EVIL" s
    | ELam (e1, e2) | ELet (_, e1, e2) | ETuple (e1, e2) | EMaybe (e1, e2) -> evil_rule_in_subtree e1 || evil_rule_in_subtree e2
    | ELamMulti (delta, ret) -> List.fold_right (fun (e, _) b -> evil_rule_in_subtree e || b) delta false || evil_rule_in_subtree ret
    | EApp _ -> true (* EApp is only used in evil rules, regular App rule has been replaced with AppExt *)
    | EAppMulti (e1, args) -> List.fold_right (fun e b -> evil_rule_in_subtree e || b) args false || evil_rule_in_subtree e1
    | EPatternMatch (list, base, step, _, _) -> evil_rule_in_subtree list || evil_rule_in_subtree base || evil_rule_in_subtree step
    end