{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | uc `elem` "AEILNORSTU" = 1
  | uc `elem` "BCMP"       = 3
  | uc `elem` "DG"         = 3
  | uc `elem` "HVWY"       = 4
  | uc `elem` "K"          = 5
  | uc `elem` "JX"         = 8
  | uc `elem` "QZ"         = 10
  | otherwise              = 0
  where uc = toUpper c

scoreString :: String -> Score
scoreString str = foldl mappend mempty (map score str)
