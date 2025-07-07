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

let welltypedProgram = ((||)) ((fst) (((compose) (((fun  x2 -> snd)) ((fst) ((1,(~-))) ) ) ((fun x3 -> ((List.length) ([]) ,let y4 = (x3) in (y4)))) (((<)) (((fun  x4 -> x4)) ((if (false) then (0) else (0))) ) (((~-)) (((+)) (1) (1) ) ) ) ,(cons) (((fun id -> id)) (((+)) (((~-)) (1) ) (((~-)) (1) ) ) ) ((safeTail) ((if ((if (false) then (false) else (true))) then ((List.map) ((fun id -> id)) ([]) ) else ((safeTail) ([]) ))) ) )) ) (((&&)) (false) (((=)) ((compose) ((fun x5 -> (if (((&&)) (false) (false) ) then (((fun id -> id)) (false) ) else (((=)) (true) (false) )))) ((fun id -> id)) ((~-)) ) ((compose) ((fun x7 -> (fst) ((x7,snd)) )) ((fun x8 -> (snd) ((true,x8)) )) (((&&)) (((&&)) (((=)) (false) (false) ) (true) ) ((if (((fun id -> id)) (true) ) then (true) else (((<)) (1) (1) ))) ) ) ) ) 

let illtypedProgram  = ((||)) ((fst) (((compose) (((fun  x2 -> snd)) ((fst) ((1,(~-))) ) ) ((fun x3 -> ((List.length) ([]) ,let y4 = (x3) in (y4)))) (((<)) (((fun  x4 -> x4)) ((if (false) then (0) else (0))) ) (((~-)) (((+)) (((cons) (((fun id -> id)) (((<)) ((fst) ((1,(if (true) then ((=)) else ((=))))) ) ((List.length) ((List.filter) (fst) (((fun id -> id)) ([]) ) ) ) ) ) (((fun id -> id)) (((fun  x20 x21 x23 x24 -> ((@)) ((cons) (x20) (x21) ) ((compose) (x23) ((&&)) (x24) ) )) ((if (((fun id -> id)) (((||)) (true) (true) ) ) then ((snd) (((@),true)) ) else (let y26 = (((=)) (true) (true) ) in (((||)) (true) (y26) )))) (((@)) ((compose) ((fun x25 -> [])) ((fun id -> id)) ((~-)) ) ((List.filter) (let y28 = ((fun id -> id)) in (y28)) (let y29 = ([]) in (y29)) ) ) ((if ((if ((if (false) then (false) else (false))) then ((begin match [] with | [] -> true; | (z43::z44) -> z43 end)) else (((=)) (true) (true) ))) then ((begin match (compose) ((fun id -> id)) (safeHead) ([])  with | None -> (fun (x40 : (bool -> bool)) -> []); | Some x -> x end)) else ((fun (x48 : (bool -> bool)) -> (cons) (true) ([]) )))) ((fst) ((((fun id -> id)) (false) ,(safeHead) ([]) )) ) ) ) ) ((safeTail) ((fst) (((compose) ((compose) ((fun x16 -> fst)) ((fun x17 -> (+))) (cons) ) ((fun id -> id)) (((List.filter) ((fun id -> id)) ([]) ,((-)) (1) (1) )) ,(fst) ((((~-)) (0) ,(if (false) then (true) else (false)))) )) ) )) (1) ) ) ) ,(cons) (((fun id -> id)) (((+)) (((~-)) (1) ) (((~-)) (1) ) ) ) ((safeTail) ((if ((if (false) then (false) else (true))) then ((List.map) ((fun id -> id)) ([]) ) else ((safeTail) ([]) ))) ) )) ) (((&&)) (false) (((=)) ((compose) ((fun x5 -> (if (((&&)) (false) (false) ) then (((fun id -> id)) (false) ) else (((=)) (true) (false) )))) ((fun id -> id)) ((~-)) ) ((compose) ((fun x7 -> (fst) ((x7,snd)) )) ((fun x8 -> (snd) ((true,x8)) )) (((&&)) (((&&)) (((=)) (false) (false) ) (true) ) ((if (((fun id -> id)) (true) ) then (true) else (((<)) (1) (1) ))) ) ) ) ) 