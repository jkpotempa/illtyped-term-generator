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

let welltypedProgram = let y1 = (List.filter) in (true)

let illtypedProgram  = let y1 = (List.filter) in ((compose) (((fun  x1 -> (fun id -> id))) (((~-)) (((fun id -> id)) (((+)) ((compose) ((if (true) then (fst) else (snd))) ((fun id -> id)) ((0,0)) ) ((fst) ((0,(fun id -> id))) ) ) ) ) ) (((&&)) (let y6 = ((+)) in (true)) ((if (((fun id -> id)) ((compose) ((snd) (((&&),(fun id -> id))) ) (let y7 = ((compose) (safeHead) ((fun id -> id)) ([]) ) in ((begin match y7 with | None -> (fun id -> id); | Some x -> x end))) ((fst) ((false,1)) ) ) ) then (let y15 = ((List.filter) ((snd) ((List.filter,snd)) ) (((fun  x14 -> x14)) (((fun id -> id)) ([]) ) ) ) in (((=)) ((snd) ((0,false)) ) (((fun id -> id)) (true) ) )) else ((begin match (if (((&&)) (((fun id -> id)) (true) ) (((||)) (false) (false) ) ) then (((@)) (((fun id -> id)) ([]) ) ((if (true) then ([]) else ([]))) ) else ((fst) (([],safeHead)) )) with | [] -> false; | (z23::z24) -> ((||)) ((if ((if (false) then (true) else (false))) then (((||)) (false) (true) ) else (((<)) (z23) (z23) ))) ((snd) ((z23,true)) )  end)))) ) ((begin match (List.map) ((fun x22 -> (<))) ((snd) (((snd) (((~-),(+))) ,(snd) (((-),[])) )) )  with | [] -> ((+)) ((compose) ((~-)) ((fun x23 -> x23)) (((~-)) (((fun  x24 -> x24)) (((+)) (0) (0) ) ) ) ) ((fst) ((((fun id -> id)) (((fun id -> id)) (0) ) ,(snd) (((@),(<))) )) ) ; | (z45::z46) -> let y28 = ((compose) ((fun x26 -> let y30 = (x26) in ((safeTail) (x26) ))) ((compose) ((begin match [] with | [] -> List.map; | (z55::z56) -> List.map end)) ((+)) ((begin match let y35 = (List.map) in ([]) with | [] -> ((-)) (0) (0) ; | (z57::z58) -> ((fun id -> id)) (1)  end)) ) ((cons) (((~-)) (((-)) (1) (0) ) ) ((compose) (let y38 = (List.length) in (safeTail)) ((fun x31 -> [])) (List.length) ) ) ) in (((-)) (((+)) ((begin match (safeHead) (y28)  with | None -> ((+)) (0) (1) ; | Some x -> x end)) ((fst) ((0,snd)) ) ) (((fun  x25 -> (compose) (List.length) (safeTail) (z46) )) (((-)) (((~-)) (0) ) ((if (false) then (0) else (0))) ) ) ) end)) )