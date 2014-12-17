{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits = go []
  where
    go digits int
      | int > 0 = go (digit:digits) remaining
      | otherwise = digits
      where
        remaining = quot int 10
        digit = mod int 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse
  where
    doubleEveryOtherLeft = zipWith ($) (cycle [id, (*2)])

sumDigits :: [Integer] -> Integer
sumDigits arr = sumArr $ map (sumArr . toDigits) arr
  where
    sumArr = foldl (+) 0

validate :: Integer -> Bool
validate cardNumber =
  modulo == 0
  where
    modulo = mod (calculateSum cardNumber) 10
    calculateSum = sumDigits . doubleEveryOther . toDigits
