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

let welltypedProgram = (snd) (((lt) ((snd) ((List.map,(fst) ((0,ifthenelse)) )) ) (((+)) (((fun id -> id)) ((begin match ((@)) ([]) ([])  with | [] -> (begin match [] with | [] -> 1; | (z9::z10) -> z9 end); | (z3::z4) -> let y3 = ((fun id -> id)) in (0) end)) ) ((if ((if ((begin match [] with | [] -> true; | (z11::z12) -> false end)) then ((begin match [] with | [] -> false; | (z15::z16) -> false end)) else (((||)) (true) (false) ))) then ((begin match (begin match [] with | [] -> []; | (z25::z26) -> z25 end) with | [] -> (List.length) ([]) ; | (z23::z24) -> (List.length) (z24)  end)) else (((~-)) (((-)) (0) (0) ) ))) ) ,true)) 

let illtypedProgram  = (snd) (((lt) ((snd) ((List.map,(fst) ((List.length,ifthenelse)) )) ) (((+)) (((fun id -> id)) ((begin match ((@)) ([]) ([])  with | [] -> (begin match [] with | [] -> 1; | (z9::z10) -> z9 end); | (z3::z4) -> let y3 = ((fun id -> id)) in (0) end)) ) ((if ((if ((begin match [] with | [] -> true; | (z11::z12) -> false end)) then ((begin match [] with | [] -> false; | (z15::z16) -> false end)) else (((||)) (true) (false) ))) then ((begin match (begin match [] with | [] -> []; | (z25::z26) -> z25 end) with | [] -> (List.length) ([]) ; | (z23::z24) -> (List.length) (z24)  end)) else (((~-)) (((-)) (0) (0) ) ))) ) ,true)) 