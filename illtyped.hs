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

welltypedProgram = (if (((Main.<)) (((+)) (((.)) ((snd) (([],snd)) ) ((if (((||)) (((&&)) (False) (False) ) (((||)) (False) (False) ) ) then ((\x3 -> (snd,1))) else (id))) (((case (map) ((:)) ([])  of [] -> (if (False) then (snd) else (fst)); (z9:z10) -> (\x7 -> (Main.<));),(fst) ((0,True)) )) ) ((case (snd) ((((||)) (False) (False) ,(safeHead) ([]) ))  of Nothing -> (id) (((-)) ((negate) (0) ) ((id) (1) ) ) ; Just x -> x;)) ) (((-)) (((+)) ((len) (((.)) ((\x8 -> [])) ((id) (safeTail) ) ((safeTail) ([]) ) ) ) ((fst) ((0,map)) ) ) (((\ x9 x10 x11 x12 -> (case (if (x10) then (x11) else (x12)) of Nothing -> x9; Just x -> x;))) (((\ x13 -> (id) (x13) )) ((negate) ((id) (0) ) ) ) (((Main.==)) (((\ x14 -> x14)) (((Main.<)) (1) (1) ) ) (((Main.==)) (((||)) (True) (False) ) (let y8 = ([]) in (True)) ) ) (((\ x15 -> (safeHead) (x15) )) (((:)) ((len) ([]) ) (((++)) ([]) ([]) ) ) ) ((snd) ((((+)) (1) (0) ,(safeHead) ([]) )) ) ) ) ) then ([]) else ((fst) ((((++)) (((.)) (((\ x16 -> safeTail)) ((len) ([]) ) ) ((fst) ((id,ifthenelse)) ) ((snd) ((fst,[])) ) ) ((id) ((fst) (([],1)) ) ) ,(||))) ))

illtypedProgram  = (if (((Main.<)) (((+)) (((.)) ((snd) ((((snd) ((negate,False)) ) (((||)) (((Main.==)) (False) (False) ) (((Main.<)) (1) (1) ) ),snd)) ) ((if (((||)) (((&&)) (False) (False) ) (((||)) (False) (False) ) ) then ((\x3 -> (snd,1))) else (id))) (((case (map) ((:)) ([])  of [] -> (if (False) then (snd) else (fst)); (z9:z10) -> (\x7 -> (Main.<));),(fst) ((0,True)) )) ) ((case (snd) ((((||)) (False) (False) ,(safeHead) ([]) ))  of Nothing -> (id) (((-)) ((negate) (0) ) ((id) (1) ) ) ; Just x -> x;)) ) (((-)) (((+)) ((len) (((.)) ((\x8 -> [])) ((id) (safeTail) ) ((safeTail) ([]) ) ) ) ((fst) ((0,map)) ) ) (((\ x9 x10 x11 x12 -> (case (if (x10) then (x11) else (x12)) of Nothing -> x9; Just x -> x;))) (((\ x13 -> (id) (x13) )) ((negate) ((id) (0) ) ) ) (((Main.==)) (((\ x14 -> x14)) (((Main.<)) (1) (1) ) ) (((Main.==)) (((||)) (True) (False) ) (let y8 = ([]) in (True)) ) ) (((\ x15 -> (safeHead) (x15) )) (((:)) ((len) ([]) ) (((++)) ([]) ([]) ) ) ) ((snd) ((((+)) (1) (0) ,(safeHead) ([]) )) ) ) ) ) then ([]) else ((fst) ((((++)) (((.)) (((\ x16 -> safeTail)) ((len) ([]) ) ) ((fst) ((id,ifthenelse)) ) ((snd) ((fst,[])) ) ) ((id) ((fst) (([],1)) ) ) ,(||))) ))