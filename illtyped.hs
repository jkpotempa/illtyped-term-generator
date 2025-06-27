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

welltypedProgram = (\x1 -> let _ = (x1 :: [Int]) in [])

illtypedProgram  = (\x1 -> let _ = (x1 :: [Int]) in ((map) ((snd) ((True,(\x19 -> ((&&)) (((Main.==)) (x19) (False) ) (((Main.<)) (1) (1) ) ))) ) ((filter) ((\x20 -> let _ = (x20 :: Bool) in (id) ((id) ((id) (False) ) ) )) (((.)) ((if (((Main.<)) ((len) ([]) ) (((+)) (1) (1) ) ) then ((case (if (True) then ([]) else ([])) of [] -> (id) (id) ; (z17:z18) -> let y13 = ([]) in (safeTail);)) else ((\x22 -> (safeTail) (x22) )))) (fst) ((let y14 = ([]) in ((id) (y14) ),(filter) ((\x23 -> x23)) ((safeTail) ([]) ) )) ) ) ) (((.)) ((\x2 -> (safeTail) ((snd) ((len,(filter) (id) ([]) )) ) )) ((++)) ((filter) ((fst) ((((.)) ((\x12 -> x12)) ((\x14 -> id)) (((:)) (negate) ([]) ) ,((.)) ((.)) ((if (False) then ((++)) else ((++)))) ([]) )) ) ((snd) (((Main.<),let y4 = (((.)) (id) (safeHead) ([]) ) in ((case y4 of Nothing -> []; Just x -> x;)))) ) ) ))