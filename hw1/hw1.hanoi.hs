type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi remaining start placeholder end
  | remaining > 1 = moveToPlaceholder ++ moveLargest ++ moveToDestination
  | otherwise = moveLargest
  where
    moveToPlaceholder = hanoi (remaining - 1) start end placeholder
    moveLargest = [(start, end)]
    moveToDestination = hanoi (remaining - 1) placeholder start end

