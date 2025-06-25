let safeHead list = 
    match list with
    | [] -> None
    | (h::t) -> Some h

let safeTail list =
    match list with
    | [] -> []
    | (h::t) -> t

let compose f g x = f (g x)

let ifthenelse c a b = if c then a else b

let welltypedProgram = ((=)) (false) (((<)) (((-)) ((if ((if (((<)) (0) (((+)) (1) (1) ) ) then (((&&)) ((Fun.id) (false) ) (let y2 = ((&&)) in (false)) ) else (((&&)) ((begin match [] with | [] -> true; | (z5::z6) -> true end)) (true) ))) then ((List.length) ((List.cons) ((List.filter) (fst) ([]) ) ((List.cons) ([]) ([]) ) ) ) else ((compose) ((if (((&&)) (true) (true) ) then (snd) else ((fun x3 -> 1)))) ((fst) ((Fun.id,1)) ) ((ifthenelse,(begin match [] with | [] -> 0; | (z17::z18) -> z17 end))) ))) ((fst) (((List.length) ((compose) (safeTail) (safeTail) ([]) ) ,(begin match (safeHead) ([])  with | None -> (compose) (ifthenelse) (Fun.id) (false) ; | Some x -> x end))) ) ) (((-)) (((-)) (((+)) (((+)) (((~-)) (1) ) (1) ) (((+)) ((List.length) ([]) ) ((if (true) then (0) else (1))) ) ) ((compose) ((fun x7 -> 0)) ((begin match (safeTail) ([])  with | [] -> (fun x8 -> compose); | (z31::z32) -> (if (z31) then (Fun.id) else (Fun.id)) end)) ((if (((&&)) (true) (false) ) then ((Fun.id) (compose) ) else ((if (false) then (compose) else (compose))))) ) ) ((compose) ((snd) ((((=)) (false) (true) ,let y18 = (compose) in (List.length))) ) (snd) (((List.length) ((List.filter) (Fun.id) ([]) ) ,(fst) (([],safeTail)) )) ) ) ) 

let illtypedProgram  = ((=)) (false) (((<)) (((-)) ((if ((if (((<)) (0) (((+)) (((-)) (((fun  x13 -> [])) (0) ) ((Fun.id) (((~-)) (let y23 = (true) in (let y24 = ((snd) ((fst,0)) ) in (((~-)) (((-)) (0) (y24) ) ))) ) ) ) (1) ) ) then (((&&)) ((Fun.id) (false) ) (let y2 = ((&&)) in (false)) ) else (((&&)) ((begin match [] with | [] -> true; | (z5::z6) -> true end)) (true) ))) then ((List.length) ((List.cons) ((List.filter) (fst) ([]) ) ((List.cons) ([]) ([]) ) ) ) else ((compose) ((if (((&&)) (true) (true) ) then (snd) else ((fun x3 -> 1)))) ((fst) ((Fun.id,1)) ) ((ifthenelse,(begin match [] with | [] -> 0; | (z17::z18) -> z17 end))) ))) ((fst) (((List.length) ((compose) (safeTail) (safeTail) ([]) ) ,(begin match (safeHead) ([])  with | None -> (compose) (ifthenelse) (Fun.id) (false) ; | Some x -> x end))) ) ) (((-)) (((-)) (((+)) (((+)) (((~-)) (1) ) (1) ) (((+)) ((List.length) ([]) ) ((if (true) then (0) else (1))) ) ) ((compose) ((fun x7 -> 0)) ((begin match (safeTail) ([])  with | [] -> (fun x8 -> compose); | (z31::z32) -> (if (z31) then (Fun.id) else (Fun.id)) end)) ((if (((&&)) (true) (false) ) then ((Fun.id) (compose) ) else ((if (false) then (compose) else (compose))))) ) ) ((compose) ((snd) ((((=)) (false) (true) ,let y18 = (compose) in (List.length))) ) (snd) (((List.length) ((List.filter) (Fun.id) ([]) ) ,(fst) (([],safeTail)) )) ) ) ) 