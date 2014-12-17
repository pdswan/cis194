module Golf where

skips :: [a] -> [[a]]
skips xs = map (flip every xs) [1..(length xs)]

every :: Int -> [a] -> [a]
every n xs
  | length xs >= n = nth : every n rest
  | otherwise = []
  where
    nth = last . take n $ xs
    rest = drop n xs

localMaxima :: [Int] -> [Int]
localMaxima =
  foldr addLocalMaximum [] . (inGroupsOf 3)
  where
    addLocalMaximum :: [Int] -> [Int] -> [Int]
    addLocalMaximum (a:b:c:[]) m
      | a < b && b > c = b:m
      | otherwise      = m
    addLocalMaximum _ m = m

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf n xs
  | length xs >= n = (take n xs):inGroupsOf n (tail xs)
  | otherwise       = [xs]
