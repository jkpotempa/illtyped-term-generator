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

let welltypedProgram = ((fun  x1 -> x1)) (((+)) ((snd) (((snd) ((snd,((-)) (1) (0) )) ,(compose) ((begin match (safeHead) ([])  with | None -> ((fun id -> id)) ((~-)) ; | Some x -> x end)) ((if ((begin match [] with | [] -> false; | (z15::z16) -> true end)) then ((~-)) else ((fun (x7 : int) -> 1)))) ((List.length) ((if (false) then ([]) else ([]))) ) )) ) ((List.length) (((fun id -> id)) ((compose) (let y10 = ((safeHead) ([]) ) in ((begin match y10 with | None -> snd; | Some x -> x end))) ((snd) ((cons,snd)) ) ((List.filter,(safeTail,[]))) ) ) ) ) 

let illtypedProgram  = ((fun  x1 -> x1)) (((+)) ((snd) (((snd) ((snd,((-)) ((if ((List.map) ((fun x9 -> 0)) (((@)) (((fun  x10 -> (fst) ((x10,ifthenelse)) )) ((begin match (if (((||)) (false) (true) ) then ((if (false) then ([]) else ([]))) else (let y19 = ([]) in (y19))) with | [] -> (snd) ((false,[])) ; | (z31::z32) -> (snd) (((=),z32))  end)) ) ((if ((snd) (((fun id -> id),((fun id -> id)) (false) )) ) then ((snd) (((List.filter) ((fun id -> id)) ([]) ,(begin match [] with | [] -> []; | (z47::z48) -> z47 end))) ) else ((List.filter) ((begin match (if (true) then ([]) else ([])) with | [] -> (fun x13 -> x13); | (z51::z52) -> (fun (x14 : bool) -> true) end)) ((compose) ((fun id -> id)) ((begin match [] with | [] -> snd; | (z55::z56) -> z55 end)) (((@),[])) ) ))) ) ) then ((-)) else ((~-)))) (0) )) ,(compose) ((begin match (safeHead) ([])  with | None -> ((fun id -> id)) ((~-)) ; | Some x -> x end)) ((if ((begin match [] with | [] -> false; | (z15::z16) -> true end)) then ((~-)) else ((fun (x7 : int) -> 1)))) ((List.length) ((if (false) then ([]) else ([]))) ) )) ) ((List.length) (((fun id -> id)) ((compose) (let y10 = ((safeHead) ([]) ) in ((begin match y10 with | None -> snd; | Some x -> x end))) ((snd) ((cons,snd)) ) ((List.filter,(safeTail,[]))) ) ) ) ) 