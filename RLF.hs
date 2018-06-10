module RLF where

-- Funciones Base
--
ol :: [Int] -> [Int]
ol xs = (0:xs)

or :: [Int] -> [Int]
or xs = (xs ++ [0])


rl :: [Int] -> [Int]
rl (x:xs) = xs


rr :: [Int] -> [Int]
rr xs = init xs


sl :: [Int] -> [Int]
sl (x:xs) = (x + 1) : xs

sr :: [Int] -> [Int]
sr xs = let (a, b) = (init xs, last xs)
        in a ++ [b + 1]

--------------------------------------------------------------------------------
-- Funciones para aplicar los operadores avanzados

needsIterate :: [Int] -> Bool
needsIterate (x:xs) = x /= (last xs)

hasMinimumSize :: Int -> [Int] -> Bool
hasMinimumSize 0 _ = True
hasMinimumSize n [] = False
hasMinimumSize n (x:xs) = hasMinimumSize (n-1) xs
