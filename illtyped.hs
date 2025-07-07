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

welltypedProgram = ((||)) ((fst) ((((.)) (((\ x2 -> snd)) ((fst) ((1,negate)) ) ) ((\x3 -> ((len) ([]) ,let y4 = (x3) in (y4)))) (((Main.<)) (((\ x4 -> x4)) ((if (False) then (0) else (0))) ) ((negate) (((+)) (1) (1) ) ) ) ,((:)) ((id) (((+)) ((negate) (1) ) ((negate) (1) ) ) ) ((safeTail) ((if ((if (False) then (False) else (True))) then ((map) (id) ([]) ) else ((safeTail) ([]) ))) ) )) ) (((&&)) (False) (((Main.==)) (((.)) ((\x5 -> (if (((&&)) (False) (False) ) then ((id) (False) ) else (((Main.==)) (True) (False) )))) (id) (negate) ) (((.)) ((\x7 -> (fst) ((x7,snd)) )) ((\x8 -> (snd) ((True,x8)) )) (((&&)) (((&&)) (((Main.==)) (False) (False) ) (True) ) ((if ((id) (True) ) then (True) else (((Main.<)) (1) (1) ))) ) ) ) ) 

illtypedProgram  = ((||)) ((fst) ((((.)) (((\ x2 -> snd)) ((fst) ((1,negate)) ) ) ((\x3 -> ((len) ([]) ,let y4 = (x3) in (y4)))) (((Main.<)) (((\ x4 -> x4)) ((if (False) then (0) else (0))) ) ((negate) (((+)) ((((:)) ((id) (((Main.<)) ((fst) ((1,(if (True) then ((Main.==)) else ((Main.==))))) ) ((len) ((filter) (fst) ((id) ([]) ) ) ) ) ) ((id) (((\ x20 x21 x23 x24 -> ((++)) (((:)) (x20) (x21) ) (((.)) (x23) ((&&)) (x24) ) )) ((if ((id) (((||)) (True) (True) ) ) then ((snd) (((++),True)) ) else (let y26 = (((Main.==)) (True) (True) ) in (((||)) (True) (y26) )))) (((++)) (((.)) ((\x25 -> [])) (id) (negate) ) ((filter) (let y28 = (id) in (y28)) (let y29 = ([]) in (y29)) ) ) ((if ((if ((if (False) then (False) else (False))) then ((case [] of [] -> True; (z43:z44) -> z43;)) else (((Main.==)) (True) (True) ))) then ((case ((.)) (id) (safeHead) ([])  of Nothing -> (\x40 -> let _ = (x40 :: (Bool -> Bool)) in []); Just x -> x;)) else ((\x48 -> let _ = (x48 :: (Bool -> Bool)) in ((:)) (True) ([]) )))) ((fst) (((id) (False) ,(safeHead) ([]) )) ) ) ) ) ((safeTail) ((fst) ((((.)) (((.)) ((\x16 -> fst)) ((\x17 -> (+))) ((:)) ) (id) (((filter) (id) ([]) ,((-)) (1) (1) )) ,(fst) (((negate) (0) ,(if (False) then (True) else (False)))) )) ) )) (1) ) ) ) ,((:)) ((id) (((+)) ((negate) (1) ) ((negate) (1) ) ) ) ((safeTail) ((if ((if (False) then (False) else (True))) then ((map) (id) ([]) ) else ((safeTail) ([]) ))) ) )) ) (((&&)) (False) (((Main.==)) (((.)) ((\x5 -> (if (((&&)) (False) (False) ) then ((id) (False) ) else (((Main.==)) (True) (False) )))) (id) (negate) ) (((.)) ((\x7 -> (fst) ((x7,snd)) )) ((\x8 -> (snd) ((True,x8)) )) (((&&)) (((&&)) (((Main.==)) (False) (False) ) (True) ) ((if ((id) (True) ) then (True) else (((Main.<)) (1) (1) ))) ) ) ) ) 