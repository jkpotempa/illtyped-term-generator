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

let welltypedProgram = (snd) (((List.map) (List.length) ((compose) ((begin match (List.map) ((compose) (List.filter) ((<)) (1) ) ((List.map) (fst) ([]) )  with | [] -> (fun id -> id); | (z1::z2) -> (fun x6 -> (compose) (safeTail) (safeTail) ([]) ) end)) (safeTail) ((compose) (((fun  x8 -> x8)) ((fun id -> id)) ) ((fun id -> id)) ((begin match (safeTail) ([])  with | [] -> ((fun id -> id)) ([]) ; | (z5::z6) -> (begin match z5 with | [] -> z6; | (z7::z8) -> [] end) end)) ) ) ,((fun  x1 -> x1)) (true) )) 

let illtypedProgram  = (snd) (((List.map) (List.length) ((compose) ((begin match (List.map) ((compose) (List.filter) ((<)) (1) ) ((List.map) (fst) ([]) )  with | [] -> (fun id -> id); | (z1::z2) -> (fun x6 -> (compose) (safeTail) (safeTail) ([]) ) end)) (safeTail) ((compose) (((fun  x8 -> x8)) ((fun id -> id)) ) ((fun id -> id)) ((begin match (safeTail) ([])  with | [] -> ((fun id -> id)) ([]) ; | (z5::z6) -> (begin match z5 with | [] -> z6; | (z7::z8) -> z5 end) end)) ) ) ,((fun  x1 -> x1)) (true) )) 

let partial1 = begin match (safeTail) ([])  with | [] -> ((fun id -> id)) ([]) ; | (z5::z6) -> (begin match z5 with | [] -> z6; | (z7::z8) -> z5 end) end
let partial2 = 
    begin match [] with 
    | [] -> [] 
    | (h::t) -> 
        begin match h with (* h = 'a list, t = 'a list list *)
        | [] -> t
        | (_::_) -> h
        end 
    end
let test =
    begin match [] with
    | [] -> 0
    | (_::_) -> (+) false 1
    end