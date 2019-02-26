
-- Ex.1
func1' :: [Int] -> Int
func1' = product . map (subtract 2) . filter even

func2' :: Integer -> Integer
func2' = sum . filter even . takeWhile (>1) .
    iterate (\n -> if even n then n `div` 2 else 3*n + 1)

-- Ex.2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

treeLevel :: Tree a -> Integer
treeLevel Leaf           = 0
treeLevel (Node n _ _ _) = n

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr treeInsert Leaf
  where
    treeInsert x Leaf = Node 0 Leaf x Leaf
    treeInsert x (Node n left root right)
      | left > right = Node (treeLevel newRight + 1) left root newRight
      | otherwise = Node (treeLevel newLeft + 1) newLeft root right
        where
          newRight = treeInsert x right
          newLeft = treeInsert x left