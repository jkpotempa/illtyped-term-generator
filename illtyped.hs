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

welltypedProgram = ((\ x1 -> x1)) (((+)) ((snd) (((snd) ((snd,((-)) (1) (0) )) ,((.)) ((case (safeHead) ([])  of Nothing -> (id) (negate) ; Just x -> x;)) ((if ((case [] of [] -> False; (z15:z16) -> True;)) then (negate) else ((\x7 -> let _ = (x7 :: Int) in 1)))) ((len) ((if (False) then ([]) else ([]))) ) )) ) ((len) ((id) (((.)) (let y10 = ((safeHead) ([]) ) in ((case y10 of Nothing -> snd; Just x -> x;))) ((snd) (((:),snd)) ) ((filter,(safeTail,[]))) ) ) ) ) 

illtypedProgram  = ((\ x1 -> x1)) (((+)) ((snd) (((snd) ((snd,((-)) ((if ((map) ((\x9 -> 0)) (((++)) (((\ x10 -> (fst) ((x10,ifthenelse)) )) ((case (if (((||)) (False) (True) ) then ((if (False) then ([]) else ([]))) else (let y19 = ([]) in (y19))) of [] -> (snd) ((False,[])) ; (z31:z32) -> (snd) (((Main.==),z32)) ;)) ) ((if ((snd) ((id,(id) (False) )) ) then ((snd) (((filter) (id) ([]) ,(case [] of [] -> []; (z47:z48) -> z47;))) ) else ((filter) ((case (if (True) then ([]) else ([])) of [] -> (\x13 -> x13); (z51:z52) -> (\x14 -> let _ = (x14 :: Bool) in True);)) (((.)) (id) ((case [] of [] -> snd; (z55:z56) -> z55;)) (((++),[])) ) ))) ) ) then ((-)) else (negate))) (0) )) ,((.)) ((case (safeHead) ([])  of Nothing -> (id) (negate) ; Just x -> x;)) ((if ((case [] of [] -> False; (z15:z16) -> True;)) then (negate) else ((\x7 -> let _ = (x7 :: Int) in 1)))) ((len) ((if (False) then ([]) else ([]))) ) )) ) ((len) ((id) (((.)) (let y10 = ((safeHead) ([]) ) in ((case y10 of Nothing -> snd; Just x -> x;))) ((snd) (((:),snd)) ) ((filter,(safeTail,[]))) ) ) ) ) 