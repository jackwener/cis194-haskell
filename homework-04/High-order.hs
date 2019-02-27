
-- Ex.1
func1' :: [Int] -> Int
func1' = product . map (subtract 2) . filter even

func2' :: Integer -> Integer
func2' = sum . filter even . takeWhile (>1) .
    iterate (\n -> if even n then n `div` 2 else 3*n + 1)

-- Ex.2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- should produce a balanced Tree using @foldr@
foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

balancedInsert :: Int -> a -> Tree a -> Tree a
balancedInsert _ _ _ = Leaf
{-balancedInsert _ x (Node n left y right)-}
          {-| right == Leaf = Node n left y (Node (n-1) Leaf x Leaf)-}
          {-| otherwise = Node n (Node (n-1) Leaf x Leaf) y right-}
{-balancedInsert start x _ = Node  Leaf x Leaf-}

-- Ex.3

binaryXor :: Bool -> Bool -> Bool
binaryXor True False = True
binaryXor False True = True
binaryXor _ _ = False

xor :: [Bool] -> Bool
xor = foldr binaryXor False


-- map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- foldl with foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse