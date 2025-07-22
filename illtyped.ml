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

let cons x xs = x :: xs

let lt : int -> int -> bool = (<)

let eq : bool -> bool -> bool = (=)

let welltypedProgram = (if ((lt) (((+)) ((compose) ((snd) (([],snd)) ) ((if (((||)) (((&&)) (false) (false) ) (((||)) (false) (false) ) ) then ((fun x3 -> (snd,1))) else ((fun id -> id)))) (((begin match (List.map) (cons) ([])  with | [] -> (if (false) then (snd) else (fst)); | (z9::z10) -> (fun x7 -> lt) end),(fst) ((0,true)) )) ) ((begin match (snd) ((((||)) (false) (false) ,(safeHead) ([]) ))  with | None -> ((fun id -> id)) (((-)) (((~-)) (0) ) (((fun id -> id)) (1) ) ) ; | Some x -> x end)) ) (((-)) (((+)) ((List.length) ((compose) ((fun x8 -> [])) (((fun id -> id)) (safeTail) ) ((safeTail) ([]) ) ) ) ((fst) ((0,List.map)) ) ) (((fun  x9 x10 x11 x12 -> (begin match (if (x10) then (x11) else (x12)) with | None -> x9; | Some x -> x end))) (((fun  x13 -> ((fun id -> id)) (x13) )) (((~-)) (((fun id -> id)) (0) ) ) ) ((eq) (((fun  x14 -> x14)) ((lt) (1) (1) ) ) ((eq) (((||)) (true) (false) ) (let y8 = ([]) in (true)) ) ) (((fun  x15 -> (safeHead) (x15) )) ((cons) ((List.length) ([]) ) (((@)) ([]) ([]) ) ) ) ((snd) ((((+)) (1) (0) ,(safeHead) ([]) )) ) ) ) ) then ([]) else ((fst) ((((@)) ((compose) (((fun  x16 -> safeTail)) ((List.length) ([]) ) ) ((fst) (((fun id -> id),ifthenelse)) ) ((snd) ((fst,[])) ) ) (((fun id -> id)) ((fst) (([],1)) ) ) ,(||))) ))

let illtypedProgram  = (if ((lt) (((+)) ((compose) ((snd) ((((snd) (((~-),false)) ) (((||)) ((eq) (false) (false) ) ((lt) (1) (1) ) ),snd)) ) ((if (((||)) (((&&)) (false) (false) ) (((||)) (false) (false) ) ) then ((fun x3 -> (snd,1))) else ((fun id -> id)))) (((begin match (List.map) (cons) ([])  with | [] -> (if (false) then (snd) else (fst)); | (z9::z10) -> (fun x7 -> lt) end),(fst) ((0,true)) )) ) ((begin match (snd) ((((||)) (false) (false) ,(safeHead) ([]) ))  with | None -> ((fun id -> id)) (((-)) (((~-)) (0) ) (((fun id -> id)) (1) ) ) ; | Some x -> x end)) ) (((-)) (((+)) ((List.length) ((compose) ((fun x8 -> [])) (((fun id -> id)) (safeTail) ) ((safeTail) ([]) ) ) ) ((fst) ((0,List.map)) ) ) (((fun  x9 x10 x11 x12 -> (begin match (if (x10) then (x11) else (x12)) with | None -> x9; | Some x -> x end))) (((fun  x13 -> ((fun id -> id)) (x13) )) (((~-)) (((fun id -> id)) (0) ) ) ) ((eq) (((fun  x14 -> x14)) ((lt) (1) (1) ) ) ((eq) (((||)) (true) (false) ) (let y8 = ([]) in (true)) ) ) (((fun  x15 -> (safeHead) (x15) )) ((cons) ((List.length) ([]) ) (((@)) ([]) ([]) ) ) ) ((snd) ((((+)) (1) (0) ,(safeHead) ([]) )) ) ) ) ) then ([]) else ((fst) ((((@)) ((compose) (((fun  x16 -> safeTail)) ((List.length) ([]) ) ) ((fst) (((fun id -> id),ifthenelse)) ) ((snd) ((fst,[])) ) ) (((fun id -> id)) ((fst) (([],1)) ) ) ,(||))) ))