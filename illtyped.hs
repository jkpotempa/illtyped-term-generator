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

welltypedProgram = let y1 = (filter) in (True)

illtypedProgram  = let y1 = (filter) in (((.)) (((\ x1 -> id)) ((negate) ((id) (((+)) (((.)) ((if (True) then (fst) else (snd))) (id) ((0,0)) ) ((fst) ((0,id)) ) ) ) ) ) (((&&)) (let y6 = ((+)) in (True)) ((if ((id) (((.)) ((snd) (((&&),id)) ) (let y7 = (((.)) (safeHead) (id) ([]) ) in ((case y7 of Nothing -> id; Just x -> x;))) ((fst) ((False,1)) ) ) ) then (let y15 = ((filter) ((snd) ((filter,snd)) ) (((\ x14 -> x14)) ((id) ([]) ) ) ) in (((Main.==)) ((snd) ((0,False)) ) ((id) (True) ) )) else ((case (if (((&&)) ((id) (True) ) (((||)) (False) (False) ) ) then (((++)) ((id) ([]) ) ((if (True) then ([]) else ([]))) ) else ((fst) (([],safeHead)) )) of [] -> False; (z23:z24) -> ((||)) ((if ((if (False) then (True) else (False))) then (((||)) (False) (True) ) else (((Main.<)) (z23) (z23) ))) ((snd) ((z23,True)) ) ;)))) ) ((case (map) ((\x22 -> (Main.<))) ((snd) (((snd) ((negate,(+))) ,(snd) (((-),[])) )) )  of [] -> ((+)) (((.)) (negate) ((\x23 -> x23)) ((negate) (((\ x24 -> x24)) (((+)) (0) (0) ) ) ) ) ((fst) (((id) ((id) (0) ) ,(snd) (((++),(Main.<))) )) ) ; (z45:z46) -> let y28 = (((.)) ((\x26 -> let y30 = (x26) in ((safeTail) (x26) ))) (((.)) ((case [] of [] -> map; (z55:z56) -> map;)) ((+)) ((case let y35 = (map) in ([]) of [] -> ((-)) (0) (0) ; (z57:z58) -> (id) (1) ;)) ) (((:)) ((negate) (((-)) (1) (0) ) ) (((.)) (let y38 = (len) in (safeTail)) ((\x31 -> [])) (len) ) ) ) in (((-)) (((+)) ((case (safeHead) (y28)  of Nothing -> ((+)) (0) (1) ; Just x -> x;)) ((fst) ((0,snd)) ) ) (((\ x25 -> ((.)) (len) (safeTail) (z46) )) (((-)) ((negate) (0) ) ((if (False) then (0) else (0))) ) ) );)) )