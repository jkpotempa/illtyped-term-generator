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

welltypedProgram = ((Main.==)) (False) (((Main.<)) (((-)) ((if ((if (((Main.<)) (0) (((+)) (1) (1) ) ) then (((&&)) ((id) (False) ) (let y2 = ((&&)) in (False)) ) else (((&&)) ((case [] of [] -> True; (z5:z6) -> True;)) (True) ))) then ((len) (((:)) ((filter) (fst) ([]) ) (((:)) ([]) ([]) ) ) ) else (((.)) ((if (((&&)) (True) (True) ) then (snd) else ((\x3 -> 1)))) ((fst) ((id,1)) ) ((ifthenelse,(case [] of [] -> 0; (z17:z18) -> z17;))) ))) ((fst) (((len) (((.)) (safeTail) (safeTail) ([]) ) ,(case (safeHead) ([])  of Nothing -> ((.)) (ifthenelse) (id) (False) ; Just x -> x;))) ) ) (((-)) (((-)) (((+)) (((+)) ((negate) (1) ) (1) ) (((+)) ((len) ([]) ) ((if (True) then (0) else (1))) ) ) (((.)) ((\x7 -> 0)) ((case (safeTail) ([])  of [] -> (\x8 -> (.)); (z31:z32) -> (if (z31) then (id) else (id));)) ((if (((&&)) (True) (False) ) then ((id) ((.)) ) else ((if (False) then ((.)) else ((.)))))) ) ) (((.)) ((snd) ((((Main.==)) (False) (True) ,let y18 = ((.)) in (len))) ) (snd) (((len) ((filter) (id) ([]) ) ,(fst) (([],safeTail)) )) ) ) ) 

illtypedProgram  = ((Main.==)) (False) (((Main.<)) (((-)) ((if ((if (((Main.<)) (0) (((+)) (((-)) (((\ x13 -> [])) (0) ) ((id) ((negate) (let y23 = (True) in (let y24 = ((snd) ((fst,0)) ) in ((negate) (((-)) (0) (y24) ) ))) ) ) ) (1) ) ) then (((&&)) ((id) (False) ) (let y2 = ((&&)) in (False)) ) else (((&&)) ((case [] of [] -> True; (z5:z6) -> True;)) (True) ))) then ((len) (((:)) ((filter) (fst) ([]) ) (((:)) ([]) ([]) ) ) ) else (((.)) ((if (((&&)) (True) (True) ) then (snd) else ((\x3 -> 1)))) ((fst) ((id,1)) ) ((ifthenelse,(case [] of [] -> 0; (z17:z18) -> z17;))) ))) ((fst) (((len) (((.)) (safeTail) (safeTail) ([]) ) ,(case (safeHead) ([])  of Nothing -> ((.)) (ifthenelse) (id) (False) ; Just x -> x;))) ) ) (((-)) (((-)) (((+)) (((+)) ((negate) (1) ) (1) ) (((+)) ((len) ([]) ) ((if (True) then (0) else (1))) ) ) (((.)) ((\x7 -> 0)) ((case (safeTail) ([])  of [] -> (\x8 -> (.)); (z31:z32) -> (if (z31) then (id) else (id));)) ((if (((&&)) (True) (False) ) then ((id) ((.)) ) else ((if (False) then ((.)) else ((.)))))) ) ) (((.)) ((snd) ((((Main.==)) (False) (True) ,let y18 = ((.)) in (len))) ) (snd) (((len) ((filter) (id) ([]) ) ,(fst) (([],safeTail)) )) ) ) ) 