{-# Language FlexibleInstances #-}

import Data.List (intercalate)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 = 0 : 1 : [x + y | (x, y) <- zip fibs2 (drop 1 fibs2)]

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a tail) = a : streamToList tail

instance Show a => Show (Stream a) where
  show = flip (++) "..." . show . take 30 . streamToList

streamRepeat :: a -> Stream a
streamRepeat elm = Stream elm (streamRepeat elm)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a tail) = Stream (f a) (streamMap f tail)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed nextSeed seed = Stream seed (streamFromSeed nextSeed (nextSeed seed))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

evens :: Stream Integer
evens = streamFromSeed (+ 2) 2

zeros :: Stream Integer
zeros = streamRepeat 0

powersOfTwo :: Stream Integer
powersOfTwo = streamFromSeed (* 2) 1

powersOfTwoUpTo :: Integer -> [Integer]
powersOfTwoUpTo n =
  take numberToTake (streamToList powersOfTwo)
  where
    numberToTake :: Int
    numberToTake = fromIntegral $ (flooredLogBase2 n) + 1

maxPowerOfTwoThatEvenlyDivides :: Integer -> Integer
maxPowerOfTwoThatEvenlyDivides n =
  fromIntegral $ indexOfDivisor - 1
  where
    indexOfDivisor :: Int
    indexOfDivisor = length $ dropWhile (not . isEvenlyDivisibleBy n) choices

    choices :: [Integer]
    choices = reverse $ powersOfTwoUpTo n

isEvenlyDivisibleBy :: Integer -> Integer -> Bool
isEvenlyDivisibleBy dividend divisor = dividend `mod` divisor == 0

flooredLogBase2 :: Integer -> Integer
flooredLogBase2 = floor . logBase 2 . fromIntegral

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a tail) b = Stream a (interleaveStreams b tail)

integerDivide :: Fractional a => Integer -> Integer -> a
integerDivide a b = (fromIntegral a) / (fromIntegral b)

ruler :: Stream Integer
ruler = interleaveStreams zeros (streamMap maxPowerOfTwoThatEvenlyDivides evens)

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n                    = Stream n (streamRepeat 0)
  negate                           = streamMap negate
  (Stream a a') + (Stream b b')    = Stream (a + b) (a' + b')
  (Stream a a') * b_@(Stream b b') =
    Stream (a * b) (aXb' + a'Xb_)
    where
      aXb'  = (fromInteger a) * b'
      a'Xb_ = a' * b_

instance Fractional (Stream Integer) where
  a_@(Stream a a') / b_@(Stream b b') =
    Stream (a `div` b) (streamMap (divideBy b) (a' - q * b'))
    where
      q = a_ / b_
      divideBy = flip div

fibs3 :: Stream Integer
fibs3 = x / ((fromInteger 1) - x - (x * x))

{- [a b    [e f    [a*e + b*g a*f + b*h -}
 {- c d] X  g h] =  c*e + d*g c*f + d*h -}

data Matrix = Matrix Integer Integer Integer Integer
instance Show Matrix where
  show (Matrix a b c d) =
    "[" ++ show a ++ " " ++ show b ++ "\n" ++
    " " ++ show c ++ " " ++ show d ++ "]"

instance Num Matrix where
  (Matrix a b c d) * (Matrix e f g h) =
    Matrix
      (a * e + b * g) (a * f + b * h)
      (c * e + d * g) (c * f + d * h)

logarithmicFib :: Integer -> Integer
logarithmicFib n
  | n == 0 = 0
  | otherwise =
    nth
    where
      (Matrix nth _ _ _) = (Matrix 1 1 1 0) ^ n

fibs4 :: [Integer]
fibs4 = map logarithmicFib [0..]

