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

welltypedProgram = let y1 = ((len) ([]) ) in (((++)) ((filter) (id) ([]) ) (((:)) (True) ([]) ) )

illtypedProgram  = let y1 = ((len) ([]) ) in (((++)) ((filter) ((\x5 -> (snd) (((Main.<),1)) )) ([]) ) (((:)) (True) ([]) ) )