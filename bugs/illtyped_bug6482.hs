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

welltypedProgram = (snd) (((map) (len) (((.)) ((case (map) (((.)) (filter) ((Main.<)) (1) ) ((map) (fst) ([]) )  of [] -> id; (z1:z2) -> (\x6 -> ((.)) (safeTail) (safeTail) ([]) );)) (safeTail) (((.)) (((\ x8 -> x8)) (id) ) (id) ((case (safeTail) ([])  of [] -> (id) ([]) ; (z5:z6) -> (case z5 of [] -> z6; (z7:z8) -> [];);)) ) ) ,((\ x1 -> x1)) (True) )) 

illtypedProgram  = (snd) (((map) (len) (((.)) ((case (map) (((.)) (filter) ((Main.<)) (1) ) ((map) (fst) ([]) )  of [] -> id; (z1:z2) -> (\x6 -> ((.)) (safeTail) (safeTail) ([]) );)) (safeTail) (((.)) (((\ x8 -> x8)) (id) ) (id) ((case (safeTail) ([])  of [] -> (id) ([]) ; (z5:z6) -> (case z5 of [] -> z6; (z7:z8) -> z5;);)) ) ) ,((\ x1 -> x1)) (True) )) 