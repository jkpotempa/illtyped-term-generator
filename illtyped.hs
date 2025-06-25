import Distribution.Simple.Utils

main = putStrLn "executed main"

len :: [a] -> Int
len = foldr (\_ n -> n+1) 0

(<) :: Int -> Int -> Bool
(<) = (Prelude.<)

(==) :: Bool -> Bool -> Bool
(==) = (Prelude.==)

ifthenelse :: Bool -> a -> a -> a
ifthenelse c a b = if c then a else b

lam1 = let y4 = (((Main.==)) (((&&)) ((case [] of [] -> False; (z7:z8) -> z7;)) (((||)) (True) (True) ) ) (((.)) (let y13 = (map) in (id)) (snd) ((id,False)) ) ) in (((.)) ((if (let y5 = (len) in (True)) then ((if (y4) then (snd) else (fst))) else ((\x13 -> let _ = (x13 :: ((Bool -> [Int]),(Bool -> [Int]))) in lam1)))) (snd) ((id,(lam1,lam1))) )

lam2 = (\x6 -> let _ = (x6 :: [Bool]) in ((:)) (((\ x7 -> x7)) (((||)) (True) (True) ) ) ((map) ((id) (snd) ) ((map) (id) ([]) ) ) )

lam3 = (\x1 -> let _ = (x1 :: Int) in (lam2) ((id) ((map) (fst) ([]) ) ) )

welltypedProgram = ((Main.==)) (((.)) ((snd) ((negate,(case (if ((if (False) then (False) else (True))) then ((safeHead) ([]) ) else (((.)) (id) (safeHead) ([]) )) of Nothing -> (case (safeHead) ([])  of Nothing -> (\x30 -> x30); Just x -> x;); Just x -> x;))) ) ((fst) (((if (((||)) (True) ((case [] of [] -> False; (z21:z22) -> False;)) ) then (((.)) ((\x33 -> x33)) ((case [] of [] -> id; (z23:z24) -> id;)) (id) ) else ((\x34 -> let _ = (x34 :: Bool) in False))),((-)) (((.)) ((\x31 -> 0)) ((\x32 -> negate)) (((&&)) (True) (False) ) ) ((negate) (0) ) )) ) (((||)) (((.)) ((\x35 -> False)) ((\x36 -> ((.)) ((\x37 -> False)) ((\x38 -> lam2)) (((-)) (0) (1) ) )) (0) ) ((fst) (((if (let y22 = (snd) in (False)) then (((Main.<)) (0) (0) ) else (let y24 = (0) in (False))),((Main.<)) (((+)) (1) (0) ) (0) )) ) ) ) ((id) (False) ) 

illtypedProgram  = ((Main.==)) (((.)) ((snd) (((((:)) ((id) ((negate) ((case (safeTail) ((lam3) (1) )  of [] -> (len) ((map) ((Main.<)) ([]) ) ; (z85:z86) -> ((.)) ((if (False) then (fst) else (fst))) ((if (False) then (id) else (id))) ((0,0)) ;)) ) ) ((case (safeHead) ((safeTail) ((filter) ((\x67 -> let _ = (x67 :: [Int]) in False)) ((safeTail) ([]) ) ) )  of Nothing -> ((.)) (fst) ((snd) ((((.)) ((Main.<)) (negate) (1) ,(id) (fst) )) ) ((((safeTail) ([]) ,True),let y51 = ((++)) in (((||)) (True) (False) ))) ; Just x -> x;)) ) ((case ((++)) ((lam2) ((snd) (((||),(map) (id) ([]) )) ) ) ((case ((.)) ((\x41 -> x41)) ((\x42 -> (id) (x42) )) ((case ((++)) ([]) ([])  of [] -> (safeHead) ([]) ; (z41:z42) -> (safeHead) (z42) ;))  of Nothing -> (map) (((.)) ((\x39 -> fst)) (lam3) (0) ) ((map) (id) ([]) ) ; Just x -> x;))  of [] -> (fst) ((((++)) (((.)) (((.)) (id) (map) (len) ) ((\x45 -> [])) (fst) ) ((snd) ((safeHead,[])) ) ,(filter) (id) ([]) )) ; (z35:z36) -> ((.)) ((\x48 -> ((++)) ((safeTail) ((id) (x48) ) ) ((map) ((if (z35) then (fst) else (fst))) ((case z36 of [] -> []; (z55:z56) -> [];)) ) )) (safeTail) (((:)) ((case (lam3) ((if (z35) then (1) else (1)))  of [] -> (fst) ((0,z36)) ; (z61:z62) -> 1;)) ((case let y42 = ((safeHead) ([]) ) in ((id) (y42) ) of Nothing -> ((.)) (id) (fst) (([],(-))) ; Just x -> x;)) ) ;)),(case (if ((if (False) then (False) else (True))) then ((safeHead) ([]) ) else (((.)) (id) (safeHead) ([]) )) of Nothing -> (case (safeHead) ([])  of Nothing -> (\x30 -> x30); Just x -> x;); Just x -> x;))) ) ((fst) (((if (((||)) (True) ((case [] of [] -> False; (z21:z22) -> False;)) ) then (((.)) ((\x33 -> x33)) ((case [] of [] -> id; (z23:z24) -> id;)) (id) ) else ((\x34 -> let _ = (x34 :: Bool) in False))),((-)) (((.)) ((\x31 -> 0)) ((\x32 -> negate)) (((&&)) (True) (False) ) ) ((negate) (0) ) )) ) (((||)) (((.)) ((\x35 -> False)) ((\x36 -> ((.)) ((\x37 -> False)) ((\x38 -> lam2)) (((-)) (0) (1) ) )) (0) ) ((fst) (((if (let y22 = (snd) in (False)) then (((Main.<)) (0) (0) ) else (let y24 = (0) in (False))),((Main.<)) (((+)) (1) (0) ) (0) )) ) ) ) ((id) (False) ) 