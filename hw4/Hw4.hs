import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = 
  foldr (+) 0 . filter even . takeWhile (>1) . iterate next
  where
    next n = if even n then n `div` 2 else 3 * n + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert i' Leaf = Node 0 Leaf i' Leaf
insert i' (Node h l i r)
  | height r >= height l =
    let
      newSubtree = insert i' l
      in
        Node (height newSubtree + 1) newSubtree i r
  | otherwise =
    let
      newSubtree = insert i' r
      in
        Node (height newSubtree + 1) l i newSubtree

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf           = True
isBalanced (Node _ l _ r) = (abs $ height l - height r) < 2 && isBalanced l && isBalanced r

xor :: [Bool] -> Bool
xor = foldr (\next memo -> if next then not memo else memo) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\elm memo -> f elm : memo) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' iter initial xs = foldr (nextStepFn iter) id xs $ initial
  where
    nextStepFn :: (a -> b -> a) -> b -> (a -> a) -> (a -> a)
    nextStepFn iter item f = f . (flip iter item)

sieveOfSundaram :: Integer -> [Integer]
sieveOfSundaram n =
  map ((+ 1) . (* 2)) (list \\ toRemove)
  where
    list :: [Integer]
    list = [0..n]

    pairs :: [(Integer, Integer)]
    pairs = filter filterFn $ cartProd list list
      where
        filterFn :: (Integer, Integer) -> Bool
        filterFn (i, j) = (i >= 1 && j >= i)

    toRemove :: [Integer]
    toRemove = takeWhile (<= n) $ map mapFn pairs
      where
        mapFn :: (Integer, Integer) -> Integer
        mapFn (i, j) = i + j + (2 * i * j)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

