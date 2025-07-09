open Types
open Helpers
open Prettyprinter

let counter = ref 0
let counterlet = ref 0
let counterpm = ref 0
let counteralpha = ref 0
let genvar = fun () -> counter := !counter + 1; "x" ^ (string_of_int !counter)
let genlet = fun () -> counterlet := !counterlet + 1; "y" ^ (string_of_int !counterlet)
let genpm = fun () -> counterpm := !counterpm + 1; "z" ^ (string_of_int !counterpm)
let genalpha = fun () -> counteralpha := !counteralpha + 1; "alpha" ^ (string_of_int !counteralpha)

let log = ref ""

exception Out_of_steps

(* replacing the generic polymorphic variables a, b, c from the gamma context with fresh alphas *)
let substitute_fresh_alphas (t : ty) : ty =
    let updated_type = ref t in
    let fresh_alpha1 = genalpha () in
    let fresh_alpha2 = genalpha () in
    let fresh_alpha3 = genalpha () in
    updated_type := substitute (TPoly "a") (TPoly fresh_alpha1) !updated_type;
    updated_type := substitute (TPoly "b") (TPoly fresh_alpha2) !updated_type;
    updated_type := substitute (TPoly "c") (TPoly fresh_alpha3) !updated_type;
    !updated_type

(* for EvilIndir. changes type of one argument in a list of types to one of the basic types, if applicable *)
let change_argument_type (types : Types.ty list) (pi : ty_hashtbl ref) : Types.ty list option = 
    let n = List.length types in
    let order = shuffle (List.init n Fun.id) in
    let finalList = ref types in
    let foundOuter = ref false in
    for i = 0 to n-1 do
        if not !foundOuter then
        let currIndex = List.nth order i in
        let currType = List.nth types currIndex in
        let basic_types_shuffled = shuffle basic_types in
        let foundInner = ref false in
        for j = 0 to (List.length basic_types)-1 do
            if not !foundInner then
            match unify (Some ([currType, List.nth basic_types_shuffled j] @ (hashtable_to_list !pi))) (Hashtbl.create 64) with
            | None -> 
                foundInner := true; foundOuter := true; 
                finalList := (take currIndex types) @ [List.nth basic_types_shuffled j] @ (drop (currIndex+1) types)
            | Some ([], substitutions) -> () 
            | Some _ -> failwith "unification failed. there should be no constraints left"
            else ()
        done;
        else ()
    done;
    if !foundOuter then Some !finalList else None

(* generates a WELL-TYPED expression *)
let rec generate (steps : int) (expected_type : ty) (ct : ctxt) (delta_opt : ctxt ref option) (pi : ty_hashtbl ref) : expr =
    
    if (steps <= 0) then raise Out_of_steps else
    
    (* DEBUG *)
    (* print_endline (ty_to_string expected_type); *)
    (* print_endline "=== CT START ===";
    print_endline (log_ctxt ct);
    print_endline "=== CT END ==="; *)
    let applicableRules : (rule * (expr * ty)) list ref = ref [] in

    (* we need to save the state of our polymorphic context in case generation fails and we have to restore it *)
    let old_pi : ty_hashtbl = !pi in
    
    (* substitute expected_type in case it is polymorphic AND HAS ALREADY BEEN INSTANTIATED TO A CONCRETE TYPE, i.e., there's an
        entry in the polymorphic context pi with expected type in it *)
    let expected_type = lookup_ty expected_type !pi [] in
    (* print_endline (ty_to_string expected_type); *)

    let gamma_combined = gamma @ ct in
    
    let unificationCache : (ty * bool) list ref = ref [] in

    (* returns true if unification succeeds and false otherwise. if t was seen before, unify will not be called again *)
    let lookup_unification_cache (t : ty) : bool =
        try
            snd (List.find (fun (cached_type, _) -> cached_type = t) !unificationCache)
        with Not_found -> (* cache miss, perform unification *)
            begin match unify (Some ([expected_type, t] @ (hashtable_to_list !pi))) (Hashtbl.create 64) with
            | None -> unificationCache := !unificationCache @ [t, false]; false
            | Some ([], _) -> unificationCache := !unificationCache @ [t, true]; true
            | Some _ -> failwith "unification failed. there should be no constraints left"
            end
    in

    (* checking if Var is applicable *)
    let matchingVarExprs = List.filter (fun ((_, constant_type) : expr * ty) -> lookup_unification_cache constant_type) gamma_combined in 
    if steps <> 8 (* prevent "boring", size-1-programs from being generated *) then applicableRules := !applicableRules @ (List.map (fun c -> Var, c) matchingVarExprs) else ();
    
    (* DEBUG PI *)
    (* print_endline "PI";
    let pi_list = hashtable_to_list !pi in
    for i=0 to (List.length pi_list)-1 do
        let curr = List.nth pi_list i in
        print_endline ((ty_to_string (fst curr)) ^ " " ^ (ty_to_string (snd curr)));
    done;
    print_endline "ENDPI"; *)
    (* let len = Hashtbl.length !pi in
    if (len mod 100) = 0 then print_endline (string_of_int len) else (); *)

    (* checking if Lam is applicable *)
    begin match expected_type with
        (* weight the lambda rule with 6, such that the probability it gets chosen is increased. design choice *)
        | TFunc (sigma, tau) | TFuncMulti ([sigma], tau) -> applicableRules := !applicableRules @ (replicate 6 (Lam, (EVar ("ignored", []), expected_type)))
        | _ -> ()
    end;

    (* checking if Indir is applicable *)
    let matchingIndirExprs = ref [] in
    for i=0 to (List.length gamma_combined)-1 do
        let constant_type = snd (List.nth gamma_combined i) in
        begin match constant_type with
            | TFuncMulti (_, tau) -> if lookup_unification_cache tau then matchingIndirExprs := !matchingIndirExprs @ [fst (List.nth gamma_combined i), substitute_fresh_alphas constant_type] else ()
            | _ -> ()
        end
    done;
    (* weight Indir with 2. design choice *)
    applicableRules := !applicableRules @ (List.map (fun ((EVar (e, [])), t) -> Indir, (EVar (e, ct), t)) !matchingIndirExprs);
    applicableRules := !applicableRules @ (List.map (fun ((EVar (e, [])), t) -> Indir, (EVar (e, ct), t)) !matchingIndirExprs);

    (* Let is always applicable *)
    applicableRules := !applicableRules @ [Let, (EVar ("ignored", []), TPoly (genalpha ()))];

    (* PatternMatch is always applicable *)
    applicableRules := !applicableRules @ [PatternMatch, (EVar ("ignored", []), TInt)];

    (* Maybe is always applicable, however nested TMaybes lead to performance decrease *)
    begin match expected_type with
        | TMaybe _ -> ()
        | _ -> applicableRules := !applicableRules @ [(Maybe, (EVar ("ignored", []), expected_type))];
    end;

    (* checking if AppExt is applicable. dynamic generation of needed types instead of using basic types *)
    begin match delta_opt with
        | None -> applicableRules := !applicableRules @ [ ((*replicate 6*) (AppExt, (EVar ("ignored", []), expected_type))) ];
        | Some _ -> ()
    end;

    (* check if ParamExt is applicable *)
    begin match delta_opt, expected_type with
        | None, _ -> ()
        | Some _, TFuncExt _ -> ()
        | Some _, _ -> applicableRules := !applicableRules @ (replicate 6 (ParamExt, (EVar ("ignored", []), expected_type)))
    end;
    
    (* check if LamExt is applicable *)
    (* in fact, if we want to generate an extendable function, this should be the only rule we should be able to apply *)
    begin match expected_type with
        | TFuncExt (delta, ret) -> applicableRules := (*!applicableRules @*) [LamExt, (EVar ("ignored", []), expected_type)]
        | _ -> ()
    end;

    (* check if Tuple is applicable *)
    begin match expected_type with
    | TTuple _ -> applicableRules := [Tuple, (EVar ("ignored", []), expected_type)]
    | _ -> ()
    end;

    (* DEBUG *)
    (* for i=0 to (List.length !applicableRules)-1 do
        print_endline ((rule_to_string (fst (List.nth !applicableRules i))) ^ " " ^ (ty_to_string (snd (snd (List.nth !applicableRules i)))))
    done;
    print_endline "=== END APPLICABLE RULES ==="; *)

    let result : expr option ref = ref None in
    let found : bool ref = ref false in
    
    applicableRules := shuffle !applicableRules;

    for i=0 to (List.length !applicableRules)-1 do
        if !found then () else
        begin try
            let curr = List.nth !applicableRules i in
            begin match (curr) with
                | (Var, (EVar (e, _), constant_type)) -> 
                    result := Some (EVar (e, ct)); 
                    found := true;

                    let updated_type = substitute_fresh_alphas constant_type in
                    begin match unify (Some ([expected_type, updated_type] @ (hashtable_to_list !pi))) (Hashtbl.create 64) with
                    | None -> failwith "shouldn't be here. if the Var rule is applicable, unification shouldn't fail here."
                    | Some ([], substitutions) -> pi := substitutions;
                    | Some _ -> failwith "unification failed. there should be no constraints left"
                    end
                | (Lam, (_, (TFunc (sigma, tau)))) -> 
                    let newBinding = genvar () in 
                    result := Some (ELam (EVar (newBinding, [EVar (newBinding, []), sigma]), generate (steps - 1) tau ( [(EVar (newBinding, []), sigma)] @ ct) delta_opt pi)); 
                    found := true;
                | (Lam, (_, (TFuncMulti ([sigma], tau)))) -> 
                    let newBinding = genvar () in 
                    result := Some (ELam (EVar (newBinding, [EVar (newBinding, []), sigma]), generate (steps - 1) tau ( [(EVar (newBinding, []), sigma)] @ ct) delta_opt pi)); 
                    found := true;
                | (Indir, (e, (TFuncMulti (args, tau)))) -> 
                    begin match unify (Some ([tau, expected_type] @ (hashtable_to_list !pi))) (Hashtbl.create 64) with
                    | None -> failwith "shouldn't be here. if the Indir rule is applicable, unification shouldn't fail here."
                    | Some ([], substitutions) -> pi := substitutions;
                    | Some _ -> failwith "unification failed. there should be no constraints left"
                    end;

                    result := Some (EAppMulti (e, List.map (fun a -> generate (steps - 1) a ct delta_opt pi) args)); 
                    found := true
                | (App, (_, sigma)) ->
                    failwith "should not be here. App has been replaced by AppExt" 
                    (* result := Some (EApp ((generate (steps - 1) (TFunc (sigma, expected_type)) ct), (generate (steps - 1) sigma ct))); 
                    found := true; *)
                | (AppExt, _) -> 
                    let delta : ctxt ref = ref [] in
                    let func = generate (steps - 1) (TFuncExt (delta, expected_type)) ct (Some delta) pi in
                    (* once delta has been filled, generate the arguments *)
                    if (List.length !delta) = 0 then failwith "delta should have at least one argument";
                    let arguments = List.map (fun (_, arg_type) -> generate (steps - 1) arg_type ct None pi) !delta in
                    result := Some (EAppMulti (func, arguments));
                    found := true;
                | (LamExt, _) ->
                    begin match expected_type with
                    | TFuncExt (delta, tau) ->
                        let body = generate (steps - 1) tau ct (Some delta) pi in
                        if (List.length !delta) = 0 then begin ();
                            let fresh_arg = genvar () in
                            delta := !delta @ [EVar (fresh_arg, [EVar (fresh_arg, []), TInt]), TInt];
                        end else ();
                        result := Some (ELamMulti (!delta, body));
                        found := true;
                    | _ -> failwith "should not be here. LamExt is only applicable if we have an extendable function" 
                    end;
                | (ParamExt, _) -> 
                    begin match delta_opt with
                    | None -> failwith "should not be here. ParamExt is only applicable if there is a delta available"
                    | Some delta ->
                        let fresh_arg = genvar () in
                        delta := !delta @ [EVar (fresh_arg, [EVar (fresh_arg, []), expected_type]), expected_type];
                        result := Some (EVar (fresh_arg, ct @ [EVar (fresh_arg, []), expected_type]));
                        found := true;
                    end;
                | Let, (_, sigma) -> 
                    let newBinding = genlet () in
                    result := Some (ELet (EVar (newBinding, [EVar (newBinding, []), sigma]), generate (steps - 1) sigma ct delta_opt pi, generate (steps - 1) expected_type (([(EVar (newBinding, []), sigma)]) @ ct) delta_opt pi)); 
                    found := true;
                | PatternMatch, _ -> 
                    let newBinding1 = genpm () in
                    let newBinding2 = genpm () in
                    let listType = genalpha () in
                    let list_expr = generate (steps - 1) (TList (TPoly listType)) ct delta_opt pi in
                    let base_expr = generate (steps - 1) expected_type ct delta_opt pi in
                    let step_expr = generate (steps - 1) expected_type (ct @ [EVar (newBinding1, []), TPoly listType; EVar (newBinding2, []), TList (TPoly listType)]) delta_opt pi in
                    result := Some (EPatternMatch (list_expr, base_expr, step_expr, EVar (newBinding1, [EVar (newBinding1, []), TPoly listType]), EVar (newBinding2, [EVar (newBinding2, []), TList (TPoly listType)]))); 
                    found := true;
                | Maybe, _ -> 
                    result := Some (EMaybe (generate (steps - 1) (TMaybe expected_type) ct delta_opt pi, generate (steps - 1) expected_type ct delta_opt pi)); 
                    found := true;
                | Tuple, (_, TTuple (fst_ty, snd_ty)) ->
                    result := Some (ETuple (generate (steps - 1) fst_ty ct delta_opt pi, generate (steps - 1) snd_ty ct delta_opt pi)); 
                    found := true;
                | r, (_, t) -> failwith ((rule_to_string r) ^ (ty_to_string t))
                end;
        with Out_of_steps ->
            found := false;
            pi := old_pi;
        end;
    done;

    if !found then
        begin match !result with
            | None -> failwith "Should not happen...?" 
            | Some e -> e
        end
    else (* precondition: found = false, so all applicable rules failed => we recursively fail as well *) raise Out_of_steps 

let rec recollect_constraints (e : expr ref) (evil_rule_spot : expr ref) (expected_type : ty) (evil_type : ty) : constraints =
    (* print_endline ("recollect_constraints " ^ (log_expr !e) ^ " " ^ (log_expr !evil_rule_spot) ^ " " ^ (log_ty expected_type) ^ " " ^ (log_ty evil_type) ^ " called"); *)
    begin match !e with
    | EVar (s, ct) -> 
        if e == evil_rule_spot 
        then
            [expected_type, evil_type]
        else
            (let actual_type = lookup !e ct in
            if expected_type = actual_type || String.starts_with ~prefix:"x" s (* we can't say anything about the types of arguments *) then [] else
                if is_polymorphic actual_type then [expected_type, substitute_fresh_alphas actual_type] else [expected_type, actual_type])
    | ELam (_, body) -> 
        begin match expected_type with
        | TFunc (_, ret) -> recollect_constraints (ref body) evil_rule_spot ret evil_type
        | _ -> recollect_constraints (ref body) evil_rule_spot (TPoly (genalpha ())) evil_type
        end
    | ELamMulti (_, body) -> 
        begin match expected_type with
        | TFuncMulti (_, ret) -> recollect_constraints (ref body) evil_rule_spot ret evil_type
        | _ -> failwith "expected_type of ELamMulti was not a TFuncMulti"
        end
    | EApp _ -> failwith "shouldn't be here. EApp is not used anymore by the generator"
    | EAppMulti (func, args) ->
        let res = ref [] in 
        let sigmas = ref [] in
        let functype = begin match func with
        | ELamMulti (delta, _) ->
            for i = 0 to (List.length delta)-1 do
                sigmas := !sigmas @ [TPoly (genalpha ())];
            done;
            TFuncMulti (!sigmas, expected_type)
        | EVar (s, ct) -> (* lookup, add needed constraints *) 
            let functype = lookup func ct in
            begin match functype with
            | TFuncMulti (args, ret) -> sigmas := List.map substitute_fresh_alphas args; res := [substitute_fresh_alphas ret, expected_type];
            | _ -> failwith "only a TFuncMulti can be used in an EAppMulti application"
            end;
            functype
        | _ -> failwith "unexpected EAppMulti LHS"
        end in
        assert (List.length !sigmas = List.length args);
        for i = 0 to (List.length !sigmas)-1 do
            res := !res @ (recollect_constraints (ref (List.nth args i)) evil_rule_spot (List.nth !sigmas i) evil_type);
        done;
        !res
    | ELet (freshvar, e1, e2) -> 
        let lettype = begin match freshvar with
        | EVar (x, ct) -> lookup freshvar ct
        | _ -> failwith "Fresh variable in ELet should always be an EVar"
        end in
        (recollect_constraints (ref e1) evil_rule_spot lettype evil_type) @ 
        (recollect_constraints (ref e2) evil_rule_spot expected_type evil_type)
    | EPatternMatch (list, base, step, x, xs) -> 
        let listtype = TList (TPoly (genalpha ())) in
        (recollect_constraints (ref list) evil_rule_spot listtype evil_type) @
        (recollect_constraints (ref base) evil_rule_spot expected_type evil_type) @
        (recollect_constraints (ref step) evil_rule_spot expected_type evil_type)
    | ETuple (e1, e2) ->
        let t1, t2 = begin match expected_type with
        | TTuple (s1, s2) -> s1, s2
        | _ -> TPoly (genalpha ()), TPoly (genalpha ())
        end in
        (recollect_constraints (ref e1) evil_rule_spot t1 evil_type) @
        (recollect_constraints (ref e2) evil_rule_spot t2 evil_type)
    | EMaybe (m, n) -> 
        (recollect_constraints (ref m) evil_rule_spot (TMaybe expected_type) evil_type) @
        (recollect_constraints (ref n) evil_rule_spot expected_type evil_type)
    end

let evil_rule_counter = ref 0
let evil_rule_applied = fun () -> evil_rule_counter := !evil_rule_counter + 1
(* let traces = ref (Tracemem.read_traces ()) *)

(* when used on its own, there's a non-zero chance no evil rule gets applied. 
use introduce_evil_rule_safe instead to generate surely ill-typed expressions *)
let rec introduce_evil_rule (e : expr) (tr : Tracemem.trace) (pi : ty_hashtbl ref) (root : expr) (use_constraint_recollection : bool) : expr =
    if !evil_rule_counter >= 1 then e else
    begin match e with
        | ELam (e1, e2) -> ELam (e1, introduce_evil_rule e2 (Lam :: tr) pi root use_constraint_recollection)
        | ELamMulti (args, e2) -> ELamMulti (args, introduce_evil_rule e2 (LamExt::tr) pi root use_constraint_recollection)
        | EApp (e1, e2) -> failwith "should not be here. EApp is no longer used in well-typed generation"; (*EApp ((introduce_evil_rule) e1 (App::tr) pi root use_constraint_recollection, introduce_evil_rule e2 (App::tr) pi root use_constraint_recollection)*)
        | EAppMulti (e1, args) -> 
            begin match e1 with
            | ELamMulti ([arg, arg_type], body) -> 
                if not (usesArgument arg body) then (* evil rule is not applicable *) e
                else EAppMulti (e1, List.map (fun arg -> introduce_evil_rule arg (Indir::tr) pi root use_constraint_recollection) args)
            | ELamMulti _ ->
                (* more than one argument, so they are used in the body of the lambda *) 
                EAppMulti (e1, List.map (fun arg -> introduce_evil_rule arg (Indir::tr) pi root use_constraint_recollection) args)
            | EVar (s, _) -> 
                EAppMulti (e1, List.map (fun arg -> introduce_evil_rule arg (Indir::tr) pi root use_constraint_recollection) args) 
            | _ -> failwith "tried applying something other than a lambda or variable"
            end
        | ELet (freshvar, e1, e2) -> ELet (freshvar, e1, introduce_evil_rule e2 (Let::tr) pi root use_constraint_recollection)
        | EPatternMatch (list, base, step, x, xs) -> 
            let listNew = introduce_evil_rule list (PatternMatch::tr) pi root use_constraint_recollection in
            let baseNew = introduce_evil_rule base (PatternMatch::tr) pi root use_constraint_recollection in
            let stepNew = introduce_evil_rule step (PatternMatch::tr) pi root use_constraint_recollection in
            EPatternMatch (listNew, baseNew, stepNew, x, xs)
        | ETuple (e1, e2) -> 
            let e1New = introduce_evil_rule e1 (Tuple :: tr) pi root use_constraint_recollection in
            let e2New = introduce_evil_rule e2 (Tuple :: tr) pi root use_constraint_recollection in
            ETuple (e1New, e2New)
        | EMaybe (e1, e2) -> 
            let e1New = introduce_evil_rule e1 (Maybe :: tr) pi root use_constraint_recollection in
            let e2New = introduce_evil_rule e2 (Maybe :: tr) pi root use_constraint_recollection in
            EMaybe (e1New, e2New)
        | EVar (s, ct) ->
            (* tracing system turned off, as once too many traces are stored, many programs are rejected straight away *)
            (* if contains (Var :: tr) !traces then (print_endline "trace collision!"; EVar (s, ct)) else begin  *)
            (* probablistic approach - switch out Var rule for an evil subtree with probability 30%. design choice *)
            Random.self_init ();
            let prob = Random.int 1000 in
            if (prob < 700 || !evil_rule_counter = 1) then EVar (s, ct) else begin
            (* traces := !traces @ [Var :: tr]; *)
            (* Tracemem.write_traces !traces; *)

            evil_rule_applied();
            (* print_endline (string_of_int !evil_rule_counter); *)

            let expected_type = lookup_ty (lookup e ct) !pi [] in

            (* DEBUG *)
            (* print_endline (ty_to_string expected_type);
            print_endline (log_expr e); *)

            let applicableRules : (rule * (expr * ty)) list ref = ref [] in
            
            let applicabilityCache : (ty * bool) list ref = ref [] in

            let lookup_applicability_cache (t : ty) : bool =
                (* print_endline ("looking up " ^ (ty_to_string t) ^ " in applicabilityCache\n"); *)
                log := !log ^ ("looking up " ^ (ty_to_string t) ^ " in applicabilityCache\n");
                try
                    snd (List.find (fun (cached_type, _) -> cached_type = t) !applicabilityCache)
                with Not_found -> (* cache miss, recollect constraints *)
                    let cs = recollect_constraints (ref root) (ref e) (TPoly "tau") t in

                    if List.length cs = 0 then log := !log ^ "cs empty!\n" else ();
                    for i=0 to (List.length cs) -1 do
                        let curr = List.nth cs i in
                        (* print_endline ((log_ty (fst curr)) ^ " " ^ (log_ty (snd curr)) ^ "\n"); *)
                        log := !log ^ ((log_ty (fst curr)) ^ " " ^ (log_ty (snd curr)) ^ "\n");
                    done;

                    begin match unify (Some cs) (Hashtbl.create 64) with
                    | None -> applicabilityCache := !applicabilityCache @ [t, true]; true
                    | Some ([], _) -> applicabilityCache := !applicabilityCache @ [t, false]; false
                    | Some _ -> failwith "unification failed. there should be no constraints left"
                    end
            in

            if not use_constraint_recollection then 
            (* OPTION 1. WITHOUT USING CONSTRAINT RECOLLECTION => MAY PRODUCE FALSE POSITIVES *)

            (print_endline "careful! not using constraint recollection. may produce false positives.";

            (* checking if EvilVar is applicable *)
            (* adding all of the constants from the context where the type doesn't match the expected_type seems to make EvilVar overrepresented in applicableRules. *)
            let matchingVarExprs = List.filter (fun ((_, constant_type) : expr * ty) -> 
                match unify (Some ([expected_type, constant_type] (*@ !pi*))) (Hashtbl.create 64) with
                | None -> true;
                | Some ([], substitutions) -> false;
                | Some _ -> failwith "unification failed. there should be no constraints left"
            ) (ct @ gamma) in 
            applicableRules := !applicableRules @ (List.map (fun c -> Var, c) matchingVarExprs);

            (* checking if EvilLam2 is applicable *)
            (* begin match expected_type with
                | TFunc (sigma, tau) -> applicableRules := !applicableRules @ List.map (fun (tau_prime:ty) -> (Lam, (EVar ("ignored", []), TFunc (sigma, tau_prime)))) (List.filter (fun (tau_prime:ty) -> tau_prime <> expected_type) basic_types)
                | _ -> ()
            end; *)
            begin match expected_type with
            | TFunc (sigma, tau) ->
                let matchingLamTypes = List.filter (fun tau_prime -> 
                    match unify (Some ([expected_type, tau_prime] (*@ !pi*))) (Hashtbl.create 64) with
                    | None -> true;
                    | Some ([], substitutions) -> false;
                    | Some _ -> failwith "unification failed. there should be no constraints left"
                ) basic_types in
                applicableRules := !applicableRules @ (List.map (fun tau_prime -> Lam, (EVar ("ignored", []), TFunc (sigma, tau_prime))) matchingLamTypes);
            | _ -> ()
            end;

            (* EvilApp is always applicable *)
            (* For now for simplicity, only non-functions are considered. design choice *)
            applicableRules := !applicableRules @ (List.map (fun t -> App, (EVar ("ignored", ct), t)) basic_types);
            
            (* checking if EvilIndir is applicable *)
            let matchingIndirExprs = List.filter (fun ((EVar (s, _), constant_type) : expr * ty) ->
                begin match constant_type with
                | TFuncMulti (args, tau) -> 
                    begin match unify (Some ([tau, expected_type] @ (hashtable_to_list !pi))) (Hashtbl.create 64), change_argument_type args pi with
                    | Some ([], _), Some _ -> true (* apply EvilIndir only if tau and the expected_type unify AND changing an argument is possible *)
                    | _, _ -> false
                    end
                | _ -> false
                end;
                ) (gamma) in
            applicableRules := !applicableRules @ (List.map (fun (e, TFuncMulti (args, _)) -> 
                Indir, (e, TFuncMulti (
                    begin match change_argument_type args pi with
                    | None -> failwith "Even though expr was said to be matching, changing argument type failed"
                    | Some ts -> ts
                    end, 
                expected_type))) matchingIndirExprs);
            )

            else 
            (* OPTION 2: USING THE NEW ALGORITHM TO ELIMINATE TYPE ANNOTATIONS *)
            
            (
            (* checking if EvilVar is applicable *)
            let matchingVarExprs = List.filter (fun ((_, constant_type) : expr * ty) -> lookup_applicability_cache constant_type) (gamma) in
            applicableRules := !applicableRules @ (List.map (fun c -> Var, c) matchingVarExprs);

            (* checking if EvilLam is applicable *)
            begin match expected_type with
            | TFunc (sigma, tau) ->
                let matchingLamTypes = List.filter (fun tau_prime -> lookup_applicability_cache tau_prime) basic_types in
                applicableRules := !applicableRules @ (List.map (fun tau_prime -> Lam, (EVar ("ignored", []), TFunc (sigma, tau_prime))) matchingLamTypes);
            | _ -> ()
            end;

            (* EvilApp is always applicable *)
            applicableRules := !applicableRules @ (List.map (fun t -> App, (EVar ("ignored", ct), t)) basic_types); 
            
            (* check if EvilIndir is applicable *)
            let matchingIndirExprs = List.filter (fun ((EVar (s, _), constant_type) : expr * ty) ->
                begin match constant_type with
                | TFuncMulti (args, tau) -> 
                    begin match unify (Some ([tau, expected_type] @ (hashtable_to_list !pi))) (Hashtbl.create 64), change_argument_type args pi with
                    | Some ([], _), Some _ -> true (* apply EvilIndir only if tau and the expected_type unify AND changing an argument is possible *)
                    | _, _ -> false
                    end
                | _ -> false
                end;
                ) (gamma) in
            applicableRules := !applicableRules @ (List.map (fun (e, TFuncMulti (args, _)) -> 
                Indir, (e, TFuncMulti (
                    begin match change_argument_type args pi with
                    | None -> failwith "Even though expr was said to be matching, changing argument type failed"
                    | Some ts -> ts
                    end, 
                expected_type))) matchingIndirExprs);
            ); 

            (* choosing the evil rule to be applied *)
            applicableRules := shuffle !applicableRules;
            let steps = 8 in
            (* DEBUG *)
            (* for i=0 to (List.length !applicableRules)-1 do
                print_endline ((rule_to_string (fst (List.nth !applicableRules i))) ^ " " ^ (ty_to_string (snd (snd (List.nth !applicableRules i)))))
            done; *)
            (* generate the needed subtrees with the types according to the evil rules, build an expression from it and return it *)
            begin match (List.hd !applicableRules) with
                | (Var, (EVar (e, _), _)) -> 
                    print_endline "EvilVar chosen";
                    log := !log ^ "\nEvilVar chosen";
                    EVar ("EVIL" ^ e, ct)
                | (Lam, (_, (TFunc (sigma, tau_prime)))) ->
                    print_endline "EvilLam chosen"; 
                    log := !log ^ "\nEvilLam chosen";
                    let newBinding = genvar () in 
                    (ELam (EVar ("EVIL" ^ newBinding, []), generate (steps - 1) tau_prime (([EVar (newBinding, []), sigma])) None (ref (Hashtbl.create 64))))
                | (App, (_, sigma)) -> 
                    print_endline "EvilApp chosen";
                    log := !log ^ "\nEvilApp chosen";
                    (EApp ((generate (steps - 1) sigma [] None (ref (Hashtbl.create 64))), (generate (steps - 1) sigma [] None (ref (Hashtbl.create 64)))))
                | (Indir, (EVar (s, _), (TFuncMulti (args, tau)))) -> 
                    print_endline "EvilIndir chosen";
                    log := !log ^ "\nEvilIndir chosen";
                    (EAppMulti (EVar ("EVIL" ^ s, ct), List.map (fun a -> generate (steps - 1) a [] None (ref (Hashtbl.create 64))) args))
            end
        end
        (* end *)
    end
and introduce_evil_rule_safe (e : expr) (pi : ty_hashtbl ref) (use_constraint_recollection : bool) : expr =
    evil_rule_counter := 0;
    let result : expr ref = ref (introduce_evil_rule e [] pi e use_constraint_recollection) in
    let tries : int ref = ref 0 in
    while !evil_rule_counter = 0 && !tries <= 100 do
        result := introduce_evil_rule e [] pi e use_constraint_recollection;
        tries := !tries + 1;
    done;
    if !tries >= 100 then failwith "Evil rule introduction failed. " else
    !result

let unusedArgumentCount = ref 0
let totalArgumentsCount = ref 0
let exprSize = ref 0
let totalVars = ref 0
let totalLams = ref 0
let totalLamExts = ref 0
let totalApps = ref 0
let totalTuples = ref 0
let totalMaybes = ref 0
let totalLets = ref 0
let totalPMs = ref 0
let totalEvilRuleLocations = ref 0
let validEvilRuleLocations = ref 0
let lamExtEvilRuleLocations = ref 0

let rec statTrack (e : expr) : unit =
    begin match e with
    | EVar _ -> totalVars := !totalVars + 1
    | ELam (e1, e2) ->
        exprSize := !exprSize + 1;
        totalLams := !totalLams + 1;
        totalArgumentsCount := !totalArgumentsCount + 1;
        if not (usesArgument e1 e2) then unusedArgumentCount := !unusedArgumentCount + 1 else ();
        statTrack e1;
        statTrack e2;
    | ELamMulti (delta, body) ->
        exprSize := !exprSize + 1;
        let len = List.length delta in
        if List.exists (fun (arg, _) -> not (usesArgument arg body)) delta then unusedArgumentCount := !unusedArgumentCount + 1;
        totalArgumentsCount := !totalArgumentsCount + len;
        totalLamExts := !totalLamExts + 1;
        exprSize := !exprSize + 1;
        statTrack body;
    | EAppMulti (e1, args) ->
        totalApps := !totalApps + 1;
        exprSize := !exprSize + 1;
        statTrack e1;
        for i = 0 to (List.length args) - 1 do 
            statTrack (List.nth args i)
        done;
    | EApp (e1, e2) -> statTrack e1; statTrack e2
    | ETuple (e1, e2) -> statTrack e1; statTrack e2; totalTuples := !totalTuples + 1
    | EMaybe (e1, e2) -> statTrack e1; statTrack e2; totalMaybes := !totalMaybes + 1
    | ELet (_, e1, e2) -> statTrack e1; statTrack e2; totalLets := !totalLets + 1
    | EPatternMatch (e1, e2, e3, _, _) -> statTrack e1; statTrack e2; statTrack e3; totalPMs := !totalPMs + 1
    end

let rec statTrackTotalEvilRuleLocations (e : expr) : unit =
    begin match e with
    | EVar _ -> totalEvilRuleLocations := !totalEvilRuleLocations + 1;
    | ELam (_, e1) | ELamMulti (_, e1) | ELet (_, _, e1) -> statTrackTotalEvilRuleLocations e1;
    | EApp _ -> failwith "tried tracking stats on evil rule locations in an expression where ERI happened already"
    | EAppMulti (_, args) -> List.iter statTrackTotalEvilRuleLocations args
    | EPatternMatch (e1, e2, e3, _, _) -> statTrackTotalEvilRuleLocations e1; statTrackTotalEvilRuleLocations e2; statTrackTotalEvilRuleLocations e3 
    | ETuple (e1, e2) | EMaybe (e1, e2) -> statTrackTotalEvilRuleLocations e1; statTrackTotalEvilRuleLocations e2
    end

let rec statTrackValidEvilRuleLocations (e : expr) (root : expr) : unit =
    begin match e with
    | EVar _ -> 
        let applicabilityCache : (ty * bool) list ref = ref [] in

        let lookup_applicability_cache (t : ty) : bool =
            try
                snd (List.find (fun (cached_type, _) -> cached_type = t) !applicabilityCache)
            with Not_found -> (* cache miss, recollect constraints *)
                let cs = recollect_constraints (ref root) (ref e) (TPoly "tau") t in
                begin match unify (Some cs) (Hashtbl.create 64) with
                | None -> applicabilityCache := !applicabilityCache @ [t, true]; true
                | Some ([], _) -> applicabilityCache := !applicabilityCache @ [t, false]; false
                | Some _ -> failwith "unification failed. there should be no constraints left"
                end
        in

        let matchingVarExprs = List.filter (fun ((_, constant_type) : expr * ty) -> lookup_applicability_cache constant_type) (gamma) in

        if List.length matchingVarExprs <> 0 then validEvilRuleLocations := !validEvilRuleLocations + 1;
    | ELam (_, e1) | ELamMulti (_, e1) | ELet (_, _, e1) -> statTrackValidEvilRuleLocations e1 root;
    | EApp _ -> failwith "tried tracking stats on evil rule locations in an expression where ERI happened already"
    | EAppMulti (_, args) -> List.iter (fun arg -> statTrackValidEvilRuleLocations arg root) args
    | EPatternMatch (e1, e2, e3, _, _) -> statTrackValidEvilRuleLocations e1 root; statTrackValidEvilRuleLocations e2 root; statTrackValidEvilRuleLocations e3 root
    | ETuple (e1, e2) | EMaybe (e1, e2) -> statTrackValidEvilRuleLocations e1 root; statTrackValidEvilRuleLocations e2 root
    end

let rec statTrackLamExtEvilRuleLocations (e : expr) (inLamExtSubtree : bool) : unit =
    begin match e with
    | EVar _ -> if inLamExtSubtree then lamExtEvilRuleLocations := !lamExtEvilRuleLocations + 1;
    | ELam (_, e1) | ELet (_, _, e1) -> statTrackLamExtEvilRuleLocations e1 inLamExtSubtree;
    | ELamMulti (_, e1) -> statTrackLamExtEvilRuleLocations e1 true;
    | EApp _ -> failwith "tried tracking stats on evil rule locations in an expression where ERI happened already"
    | EAppMulti (_, args) -> List.iter (fun arg -> statTrackLamExtEvilRuleLocations arg inLamExtSubtree) args
    | EPatternMatch (e1, e2, e3, _, _) -> statTrackLamExtEvilRuleLocations e1 inLamExtSubtree; statTrackLamExtEvilRuleLocations e2 inLamExtSubtree; statTrackLamExtEvilRuleLocations e3 inLamExtSubtree
    | ETuple (e1, e2) | EMaybe (e1, e2) -> statTrackLamExtEvilRuleLocations e1 inLamExtSubtree; statTrackLamExtEvilRuleLocations e2 inLamExtSubtree
    end

let statTrackEvilRuleLocations (e : expr) : unit =
    statTrackTotalEvilRuleLocations e;
    statTrackValidEvilRuleLocations e e;
    statTrackLamExtEvilRuleLocations e false

let main = 
    (* variables which can be disabled by flags *)
    let use_constraint_recollection = ref true in
    let track_statistics = ref true in
    let usage_msg = "./generator [-no-stat] [-no-constraint-recollection]" in
    let speclist = [("-no-stat", Arg.Clear track_statistics, "Do not track statistics")
                   ;("-no-constraint-recollection", Arg.Clear use_constraint_recollection, "Do not use constraint recollection")] in
    Arg.parse speclist (fun _ -> ()) usage_msg;

    (* setup *)
    (* Printexc.record_backtrace true; *)
    let totalStartTime = Sys.time() in
    let out_channel_haskell = open_out "illtyped.hs" in
    let out_channel_ocaml = open_out "illtyped.ml" in
    (* well-typed generation *)
    let expected_type = List.hd (shuffle (TFunc (TList TInt, TList TInt) :: basic_types)) in
    let steps = 8 in
    let pi = ref (Hashtbl.create 64) in
    print_endline ("Generating program of type " ^ (ty_to_string expected_type) ^ " in " ^ (string_of_int steps) ^ " steps...");
    let generationStartTime = Sys.time() in
    let e = (generate steps expected_type [] None pi) in
    let generationTime = Sys.time() -. generationStartTime in
    (* evil rule introduction *)
    let eriStartTime = Sys.time() in
    let evil = introduce_evil_rule_safe e pi !use_constraint_recollection in
    let eriTime = Sys.time() -. eriStartTime in
    (* printing final program files *)
    (* Haskell *)
    let out_program = ref ((program_prefix_haskell) ^ (" = ") ^ (prettyprint_haskell e) ^ ("\n\nilltypedProgram ")) in
    (* if not !use_constraint_recollection then out_program := !out_program ^ ":: " ^ (ty_to_string expected_type) else ();  *)
    out_program := !out_program ^ (" = ") ^ (prettyprint_haskell evil);
    Printf.fprintf out_channel_haskell "%s" !out_program;
    (* Ocaml *)
    let out_program = ref ((program_prefix_ocaml) ^ (" = ") ^ (prettyprint_ocaml e) ^ ("\n\nlet illtypedProgram ")) in
    (* if not !use_constraint_recollection then out_program := !out_program ^ ": " ^ (ty_to_string_ocaml expected_type) else ();  *)
    out_program := !out_program ^ (" = ") ^ (prettyprint_ocaml evil);
    Printf.fprintf out_channel_ocaml "%s" !out_program;

    (* tracking statistics *)
    statTrack evil;
    if !track_statistics then statTrackEvilRuleLocations e;
    let ratio = (Float.of_int ((!totalLams + !totalLamExts - !unusedArgumentCount)) /. Float.of_int (!totalLams + !totalLamExts)) in
    let argUsageStr = ("Argument usage in lambdas: " ^ (string_of_int (!totalLams + !totalLamExts - !unusedArgumentCount)) ^ "/" ^ (string_of_int (!totalLams + !totalLamExts)) ^ " = " ^ (string_of_float ratio)) in
    let exprSizeStr = ("Expression size: " ^ (string_of_int !exprSize)) in
    let generationTimeStr = ("Generation time: " ^ (string_of_float generationTime)) in
    let eriTimeStr = ("ERI time: " ^ (string_of_float eriTime)) in
    let totalTime = Sys.time() -. totalStartTime in
    let totalTimeStr = ("Total time: " ^ (string_of_float totalTime)) in
    let totalVarStr = string_of_int !totalVars in
    let totalLamStr = string_of_int !totalLams in
    let totalLamExtStr = string_of_int !totalLamExts in
    let totalAppStr = string_of_int !totalApps in
    let totalTupleStr = string_of_int !totalTuples in
    let totalMaybeStr = string_of_int !totalMaybes in
    let totalLetStr = string_of_int !totalLets in
    let totalPMStr = string_of_int !totalPMs in
    let totalEvilRuleLocationsStr = string_of_int !totalEvilRuleLocations in
    let validEvilRuleLocationsStr = string_of_int !validEvilRuleLocations in
    let lamExtEvilRuleLocationsStr = string_of_int !lamExtEvilRuleLocations in
    print_endline argUsageStr; print_endline exprSizeStr; print_endline generationTimeStr; print_endline eriTimeStr; print_endline totalTimeStr;
    (* logging *)
    let out_log = open_out "expr_log.txt" in
    let log_pi = log_ty_hashtbl !pi in
    Printf.fprintf out_log "%s" ((log_expr e) ^ "\n" ^ (log_expr evil) ^ "\nPI: " ^ log_pi ^ "\nEXPECTED TYPE: " 
        ^ (log_ty expected_type) ^ "\n" ^ argUsageStr ^ "\n" ^ exprSizeStr ^ "\n" ^ generationTimeStr ^ "\n" 
        ^ eriTimeStr ^ "\n" ^ totalTimeStr ^ "\n" ^ totalVarStr ^ "\n" ^ totalLamStr ^ "\n" ^ totalLamExtStr 
        ^ "\n" ^ totalAppStr ^ "\n" ^ totalTupleStr ^ "\n" ^ totalMaybeStr ^ "\n" ^ totalLetStr ^ "\n" ^ totalPMStr ^ "\n" 
        ^ totalEvilRuleLocationsStr ^ "\n" ^ validEvilRuleLocationsStr ^ "\n" ^ lamExtEvilRuleLocationsStr ^ "\n\n" ^ !log);
    close_out out_log;
    (* writing stats to file *)
    if !track_statistics then begin
    let found = ref false in
    for i = 1 to 250 do
        if not !found then
        let in_ratios = open_in ("stats/" ^ (string_of_int i) ^ ".csv") in
        (* if the file is too large, write to the next best one instead to avoid overhead *)
        (* one program produces 70 chars on average so a single stats file will fit about 500 lines *)
        if in_channel_length in_ratios >= 35000 then close_in in_ratios else 
            let entirefile = ref "" in
            try
            while true do
                entirefile := !entirefile ^ input_line in_ratios ^ "\n";
            done;
            with End_of_file ->
            found := true;
            close_in in_ratios;
            let out_ratios = open_out ("stats/" ^ (string_of_int i) ^ ".csv") in
            Printf.fprintf out_ratios "%s" (!entirefile ^ (string_of_float ratio) ^ "," ^ (string_of_int !exprSize) 
                ^ "," ^ (string_of_float generationTime) ^ "," ^ (string_of_float eriTime) ^ "," 
                ^ (string_of_float totalTime) ^ "," ^ (log_ty expected_type) ^ "," ^ totalVarStr ^ "," ^ totalLamStr 
                ^ "," ^ totalLamExtStr ^ "," ^ totalAppStr ^ "," ^ totalTupleStr ^ "," ^ totalMaybeStr ^ "," 
                ^ totalLetStr ^ "," ^ totalPMStr ^ "," ^ totalEvilRuleLocationsStr ^ "," ^ validEvilRuleLocationsStr 
                ^ "," ^ lamExtEvilRuleLocationsStr); 
            close_out out_ratios;
    done;
    end else ();
    close_out out_channel_haskell;
    close_out out_channel_ocaml;