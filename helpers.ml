let shuffle (elems : 'a list) : 'a list =
    Random.self_init ();
    let res : ('a * int) list = List.map (fun e -> (e, Random.bits ())) elems in
    let shuffled = List.sort (fun (_, rnd1) (_, rnd2) -> rnd1 - rnd2) res in
    List.map fst shuffled

let rec replicate (n : int) (elem : 'a) : 'a list =
    let rec helper (n : int) (elem : 'a) (acc : 'a list) =
        match n with
        | 0 -> acc
        | n -> helper (n-1) elem (elem::acc)
    in helper n elem []

let rec take (n : int) (elems : 'a list) : 'a list = 
    match n with
        | 0 -> []
        | n -> (List.hd elems) :: (take (n-1) (List.tl elems))

let rec drop (n : int) (elems : 'a list) : 'a list = 
    match n with
        | 0 -> elems
        | n -> drop (n-1) (List.tl elems) 

let contains (elem : 'a) (xs : 'a list) : bool = 
    List.exists (fun x -> x = elem) xs