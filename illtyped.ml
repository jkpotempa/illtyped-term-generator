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

let welltypedProgram = let y1 = ((List.length) ([]) ) in (((@)) ((List.filter) ((fun id -> id)) ([]) ) ((cons) (true) ([]) ) )

let illtypedProgram  = let y1 = ((List.length) ([]) ) in (((@)) ((List.filter) ((fun x5 -> (snd) (((<),1)) )) ([]) ) ((cons) (true) ([]) ) )