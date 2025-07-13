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

welltypedProgram = ((.)) ((\x1 -> False)) (snd) ((((.)) (((.)) ((\x9 -> (.))) ((id) (negate) ) (((.)) ((id) ((\x10 -> 0)) ) (negate) ((len) (((++)) ([]) ([]) ) ) ) ) ((case (id) ((if (((Main.==)) (True) (False) ) then (((.)) (safeHead) (safeTail) ([]) ) else ((safeHead) ([]) )))  of Nothing -> let y5 = ((case (safeHead) ([])  of Nothing -> (Main.==); Just x -> x;)) in ((+)); Just x -> x;)) (let y20 = (0) in (0)) ,((:)) ((len) ((snd) (((len) ([]) ,(map) (negate) ([]) )) ) ) ((case (safeTail) ((map) (id) (((:)) (filter) ([]) ) )  of [] -> ((.)) ((id) ((if (True) then (safeTail) else (id))) ) (((.)) (filter) ((\x5 -> x5)) ((\x6 -> let _ = (x6 :: Int) in False)) ) (((:)) (((-)) (0) (1) ) ((id) ([]) ) ) ; (z1:z2) -> [];)) )) 

illtypedProgram  = ((.)) ((\x1 -> ((len) (((.)) (id) (id) ([]) ) ) (((.)) (len) ((if (True) then (safeTail) else (id))) ((filter) (fst) ([]) ) ))) (snd) ((((.)) (((.)) ((\x9 -> (.))) ((id) (negate) ) (((.)) ((id) ((\x10 -> 0)) ) (negate) ((len) (((++)) ([]) ([]) ) ) ) ) ((case (id) ((if (((Main.==)) (True) (False) ) then (((.)) (safeHead) (safeTail) ([]) ) else ((safeHead) ([]) )))  of Nothing -> let y5 = ((case (safeHead) ([])  of Nothing -> (Main.==); Just x -> x;)) in ((+)); Just x -> x;)) (let y20 = (0) in (0)) ,((:)) ((len) ((snd) (((len) ([]) ,(map) (negate) ([]) )) ) ) ((case (safeTail) ((map) (id) (((:)) (filter) ([]) ) )  of [] -> ((.)) ((id) ((if (True) then (safeTail) else (id))) ) (((.)) (filter) ((\x5 -> x5)) ((\x6 -> let _ = (x6 :: Int) in False)) ) (((:)) (((-)) (0) (1) ) ((id) ([]) ) ) ; (z1:z2) -> [];)) )) 