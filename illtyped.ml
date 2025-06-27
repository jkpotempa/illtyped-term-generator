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

let welltypedProgram = (fun (x1 : (int) list) -> [])

let illtypedProgram  = (fun (x1 : (int) list) -> ((List.map) ((snd) ((true,(fun x19 -> ((&&)) (((=)) (x19) (false) ) (((<)) (1) (1) ) ))) ) ((List.filter) ((fun (x20 : bool) -> (Fun.id) ((Fun.id) ((Fun.id) (false) ) ) )) ((compose) ((if (((<)) ((List.length) ([]) ) (((+)) (1) (1) ) ) then ((begin match (if (true) then ([]) else ([])) with | [] -> (Fun.id) (Fun.id) ; | (z17::z18) -> let y13 = ([]) in (safeTail) end)) else ((fun x22 -> (safeTail) (x22) )))) (fst) ((let y14 = ([]) in ((Fun.id) (y14) ),(List.filter) ((fun x23 -> x23)) ((safeTail) ([]) ) )) ) ) ) ((compose) ((fun x2 -> (safeTail) ((snd) ((List.length,(List.filter) (Fun.id) ([]) )) ) )) ((@)) ((List.filter) ((fst) (((compose) ((fun x12 -> x12)) ((fun x14 -> Fun.id)) ((List.cons) ((~-)) ([]) ) ,(compose) (compose) ((if (false) then ((@)) else ((@)))) ([]) )) ) ((snd) (((<),let y4 = ((compose) (Fun.id) (safeHead) ([]) ) in ((begin match y4 with | None -> []; | Some x -> x end)))) ) ) ))