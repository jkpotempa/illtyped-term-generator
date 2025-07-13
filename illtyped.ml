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

let welltypedProgram = (compose) ((fun x1 -> false)) (snd) (((compose) ((compose) ((fun x9 -> compose)) (((fun id -> id)) ((~-)) ) ((compose) (((fun id -> id)) ((fun x10 -> 0)) ) ((~-)) ((List.length) (((@)) ([]) ([]) ) ) ) ) ((begin match ((fun id -> id)) ((if (((=)) (true) (false) ) then ((compose) (safeHead) (safeTail) ([]) ) else ((safeHead) ([]) )))  with | None -> let y5 = ((begin match (safeHead) ([])  with | None -> (=); | Some x -> x end)) in ((+)); | Some x -> x end)) (let y20 = (0) in (0)) ,(cons) ((List.length) ((snd) (((List.length) ([]) ,(List.map) ((~-)) ([]) )) ) ) ((begin match (safeTail) ((List.map) ((fun id -> id)) ((cons) (List.filter) ([]) ) )  with | [] -> (compose) (((fun id -> id)) ((if (true) then (safeTail) else ((fun id -> id)))) ) ((compose) (List.filter) ((fun x5 -> x5)) ((fun (x6 : int) -> false)) ) ((cons) (((-)) (0) (1) ) (((fun id -> id)) ([]) ) ) ; | (z1::z2) -> [] end)) )) 

let illtypedProgram  = (compose) ((fun x1 -> ((List.length) ((compose) ((fun id -> id)) ((fun id -> id)) ([]) ) ) ((compose) (List.length) ((if (true) then (safeTail) else ((fun id -> id)))) ((List.filter) (fst) ([]) ) ))) (snd) (((compose) ((compose) ((fun x9 -> compose)) (((fun id -> id)) ((~-)) ) ((compose) (((fun id -> id)) ((fun x10 -> 0)) ) ((~-)) ((List.length) (((@)) ([]) ([]) ) ) ) ) ((begin match ((fun id -> id)) ((if (((=)) (true) (false) ) then ((compose) (safeHead) (safeTail) ([]) ) else ((safeHead) ([]) )))  with | None -> let y5 = ((begin match (safeHead) ([])  with | None -> (=); | Some x -> x end)) in ((+)); | Some x -> x end)) (let y20 = (0) in (0)) ,(cons) ((List.length) ((snd) (((List.length) ([]) ,(List.map) ((~-)) ([]) )) ) ) ((begin match (safeTail) ((List.map) ((fun id -> id)) ((cons) (List.filter) ([]) ) )  with | [] -> (compose) (((fun id -> id)) ((if (true) then (safeTail) else ((fun id -> id)))) ) ((compose) (List.filter) ((fun x5 -> x5)) ((fun (x6 : int) -> false)) ) ((cons) (((-)) (0) (1) ) (((fun id -> id)) ([]) ) ) ; | (z1::z2) -> [] end)) )) 