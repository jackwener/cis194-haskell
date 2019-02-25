module Golf where

---------------------- exercise 1 --------------------
{-
skips :: [a] -> [[a]]
skips xs = map (map (xs !!)) picks where
  picks = [[m, 2*m + 1 .. end] | m <- [0 .. end]] where
    end = length xs - 1
-}

skips :: [a]->[[a]]
skips lst = [each i lst | i <- [1..length lst]]

each :: Int->[a]->[a]
each n lst = [lst !! (i-1) | i <- [n,2*n..length lst]]

---------------------- exercise 2 --------------------


localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs)
  | y>x && y>z = y:localMaxima(z:zs)
  | otherwise = localMaxima(y:z:zs)
localMaxima _ = []

---------------------- exercise 3 --------------------

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]