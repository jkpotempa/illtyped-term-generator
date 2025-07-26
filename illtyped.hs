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

welltypedProgram = (snd) ((((Main.<)) ((snd) ((map,(fst) ((0,ifthenelse)) )) ) (((+)) ((id) ((case ((++)) ([]) ([])  of [] -> (case [] of [] -> 1; (z9:z10) -> z9;); (z3:z4) -> let y3 = (id) in (0);)) ) ((if ((if ((case [] of [] -> True; (z11:z12) -> False;)) then ((case [] of [] -> False; (z15:z16) -> False;)) else (((||)) (True) (False) ))) then ((case (case [] of [] -> []; (z25:z26) -> z25;) of [] -> (len) ([]) ; (z23:z24) -> (len) (z24) ;)) else ((negate) (((-)) (0) (0) ) ))) ) ,True)) 

illtypedProgram  = (snd) ((((Main.<)) ((snd) ((map,(fst) ((len,ifthenelse)) )) ) (((+)) ((id) ((case ((++)) ([]) ([])  of [] -> (case [] of [] -> 1; (z9:z10) -> z9;); (z3:z4) -> let y3 = (id) in (0);)) ) ((if ((if ((case [] of [] -> True; (z11:z12) -> False;)) then ((case [] of [] -> False; (z15:z16) -> False;)) else (((||)) (True) (False) ))) then ((case (case [] of [] -> []; (z25:z26) -> z25;) of [] -> (len) ([]) ; (z23:z24) -> (len) (z24) ;)) else ((negate) (((-)) (0) (0) ) ))) ) ,True)) 