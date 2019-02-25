
-- Ex.1
func1' :: [Int] -> Int
func1' = product . map (subtract 2) . filter even

func2' :: Integer -> Integer
func2' = sum . filter even . takeWhile (>1) .
    iterate (\n -> if even n then n `div` 2 else 3*n + 1)

-- Ex.2

data Tree a = Leafx
            | Node Int (Tree a) a (Tree a)
            deriving (show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf