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

lam1 = (snd) (((lam2) (((:)) (1) ([]) ) ,((.)) ((\x6 -> x6)) ((\x10 -> lam1)) (((Main.<)) (1) (1) ) )) 

lam2 = (if ((lam3) ((id) ((id) (False) ) ) ) then ((\x3 -> ((:)) (let y2 = (0) in (y2)) ((case x3 of [] -> []; (z3:z4) -> z4;)) )) else ((snd) ((((Main.==)) (False) (True) ,(if (True) then (lam2) else (safeTail)))) ))

lam3 = ((\ x1 -> x1)) ((\x2 -> (if (True) then (((&&)) (x2) (False) ) else (((Main.==)) (True) (x2) )))) 

welltypedProgram = (case let y11 = ((case (fst) (([],(negate) (1) ))  of [] -> ((.)) ((if ((case (safeHead) ([])  of Nothing -> ((||)) (True) (False) ; Just x -> x;)) then ((case ((++)) ([]) ([])  of [] -> ((.)) ((++)) (id) ([]) ; (z39:z40) -> safeTail;)) else ((fst) ((safeTail,(.))) ))) (((\ x28 -> snd)) ((fst) ((0,(:))) ) ) ((((:)) ((safeTail) ([]) ) ([]) ,(snd) ((lam1,[])) )) ; (z27:z28) -> ((:)) ((case (lam2) ((if (False) then (z28) else ([])))  of [] -> (fst) ((safeHead,True)) ; (z47:z48) -> (id) ((id) (safeHead) ) ;)) ((filter) ((id) ((\x51 -> let _ = (x51 :: ([Int] -> Maybe Int)) in True)) ) ((snd) ((False,[])) ) ) ;)) in ((id) (((.)) ((case (id) ((safeHead) (y11) )  of Nothing -> safeHead; Just x -> x;)) ((case (safeHead) ((map) (id) ([]) )  of Nothing -> safeTail; Just x -> x;)) ((map) (let y12 = ((lam2) ([]) ) in (id)) ((fst) (([],(&&))) ) ) ) ) of Nothing -> (negate) ((if (((&&)) (((||)) ((fst) ((True,id)) ) (((Main.<)) ((len) ([]) ) (let y6 = (lam3) in (0)) ) ) ((lam3) (((Main.==)) (False) ((id) (False) ) ) ) ) then ((fst) ((((-)) (((-)) (1) (0) ) ((negate) (0) ) ,(lam3) (((Main.==)) (True) (True) ) )) ) else ((snd) (((fst) (((||),filter)) ,((+)) ((negate) (0) ) ((lam1) (True) ) )) ))) ; Just x -> x;)

illtypedProgram  = (case let y11 = ((case (fst) (([],(negate) (1) ))  of [] -> ((.)) ((if ((case (safeHead) ([])  of Nothing -> ((||)) (True) (False) ; Just x -> x;)) then ((case ((++)) ([]) ([])  of [] -> ((.)) ((++)) (id) ([]) ; (z39:z40) -> safeTail;)) else ((fst) ((safeTail,(.))) ))) (((\ x28 -> snd)) ((fst) ((0,(:))) ) ) ((((:)) ((safeTail) ([]) ) ([]) ,(snd) ((lam1,[])) )) ; (z27:z28) -> ((:)) ((case (lam2) ((if (False) then (z28) else ([])))  of [] -> (fst) ((safeHead,True)) ; (z47:z48) -> (id) ((id) (safeHead) ) ;)) ((filter) ((id) ((\x51 -> let _ = (x51 :: ([Int] -> Maybe Int)) in True)) ) ((snd) ((False,[])) ) ) ;)) in ((id) (((.)) ((case (id) ((safeHead) (y11) )  of Nothing -> (let y37 = ((len) (((.)) ((\x83 -> (filter) ((case [] of [] -> fst; (z75:z76) -> z75;)) (((++)) ([]) (x83) ) )) ((id) ((if ((if (False) then (False) else (True))) then (((.)) (filter) (id) (fst) ) else ((\x91 -> [])))) ) ((filter) (((.)) ((\x92 -> snd)) ((\x93 -> snd)) (((&&)) (True) (True) ) ) ([]) ) ) ) in ((id) ([]) )) (let y29 = ((snd) (((map) ((if (((&&)) (False) (False) ) then (safeHead) else ((if (True) then (safeHead) else (safeHead))))) ((id) (((:)) ([]) ([]) ) ) ,((||)) (((Main.<)) ((negate) (0) ) (0) ) ((snd) ((safeHead,True)) ) )) ) in ((snd) (((lam2) ((fst) (([],(||))) ) ,(filter) (((.)) ((if (y29) then ((||)) else ((||)))) ((\x54 -> True)) (negate) ) ((id) ([]) ) )) )); Just x -> x;)) ((case (safeHead) ((map) (id) ([]) )  of Nothing -> safeTail; Just x -> x;)) ((map) (let y12 = ((lam2) ([]) ) in (id)) ((fst) (([],(&&))) ) ) ) ) of Nothing -> (negate) ((if (((&&)) (((||)) ((fst) ((True,id)) ) (((Main.<)) ((len) ([]) ) (let y6 = (lam3) in (0)) ) ) ((lam3) (((Main.==)) (False) ((id) (False) ) ) ) ) then ((fst) ((((-)) (((-)) (1) (0) ) ((negate) (0) ) ,(lam3) (((Main.==)) (True) (True) ) )) ) else ((snd) (((fst) (((||),filter)) ,((+)) ((negate) (0) ) ((lam1) (True) ) )) ))) ; Just x -> x;)