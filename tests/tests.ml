open Helpers
open Types

let resolveTest (testName : string) (expected : 'a) (actual : 'a) : unit = 
    if expected = actual then print_endline (testName ^ "PASSED") else print_endline (testName ^ "FAILED")
  
let main =
    resolveTest "Test right_associate 1: " (TFunc (TInt, TBool)) (right_associate (TFuncMulti ([TInt], TBool)));
    resolveTest "Test right_associate 2: " (TFunc (TMaybe TInt, TFunc (TList TBool, TBool))) (right_associate (TFuncExt (ref [EVar ("ignored", [], -1), TMaybe TInt; EVar ("ignored", [], -1), TList TBool], TBool)));
    resolveTest "Test substitute 1: " (TTuple (TPoly "a", TPoly "b")) (substitute TInt (TPoly "a") (TTuple (TPoly "a", TPoly "b")));
    resolveTest "Test substitute 2: " (TList (TFunc (TInt, TInt))) (substitute TBool TInt (TList (TFunc (TInt, TInt))));
    
    (* {([Int],α),(β,[α])} *)
    let expected_ht = Hashtbl.create 4 in
    let actual_ht = Hashtbl.create 4 in
    Hashtbl.replace expected_ht (TPoly "alpha") (TList TInt);
    Hashtbl.replace expected_ht (TPoly "beta") (TList (TList TInt));
    resolveTest "Test unify 1: " (Some ([], expected_ht)) (unify (Some [TList TInt, TPoly "alpha"; TPoly "beta", TList (TPoly "alpha")]) actual_ht);
    (* {α → α,Int} *)
    let actual_ht = Hashtbl.create 4 in
    resolveTest "Test unify 2: " (None) (unify (Some [TInt, TFunc (TPoly "alpha", TPoly "alpha")]) actual_ht);
    (* {Maybe α,Maybe (Int, Bool)} *)
    let expected_ht = Hashtbl.create 4 in
    let actual_ht = Hashtbl.create 4 in
    Hashtbl.replace expected_ht (TPoly "alpha") (TTuple (TInt, TBool));
    resolveTest "Test unify 3: " (Some ([], expected_ht)) (unify (Some [TMaybe (TPoly "alpha"), TMaybe (TTuple (TInt, TBool))]) actual_ht)