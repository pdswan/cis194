{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*>), pure)

import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = (/ (fromIntegral numInvasions)) <$> (fromIntegral . length <$> wonByAttackers <$> invasions)
  where
    numInvasions :: Int
    numInvasions = 1000

    invasions :: Rand StdGen [Battlefield]
    invasions = replicateM n (invade battlefield)

    wonByAttackers :: [Battlefield] -> [Battlefield]
    wonByAttackers = filter attackersHaveWon

attackersHaveWon :: Battlefield -> Bool
attackersHaveWon (Battlefield _ defenders) = defenders == 0

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(Battlefield attackers defenders)
  | defenders == 0 = return battlefield
  | attackers < 2 = return battlefield
  | otherwise = invade =<< battle battlefield

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield@(Battlefield attackers defenders) =
  reconcileCasualties battlefield <$> casualties
  where
    casualties = computeCasualties
      <$> (rollFor $ attacking attackers)
      <*> (rollFor $ defending defenders)

reconcileCasualties :: Battlefield -> (Army, Army) -> Battlefield
reconcileCasualties (Battlefield attackers defenders) (attackerCasualties, defenderCasualties) =
  Battlefield (attackers - attackerCasualties) (defenders - defenderCasualties)

computeCasualties :: [DieValue] -> [DieValue] -> (Army, Army)
computeCasualties attackerRolls defenderRolls =
  foldr addPairs (0, 0) casualties
  where
    casualties = map
      computeCasualty
      (zip attackerRolls defenderRolls)

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (a, b) (c, d) = (a + c, b + d)

computeCasualty :: (DieValue, DieValue) -> (Army, Army)
computeCasualty (attackingRoll, defendingRoll) =
  if attackingRoll > defendingRoll then (0, 1) else (1, 0)

rollFor :: Army -> Rand StdGen [DieValue]
rollFor army = sortDesc <$> dice army

attacking :: Army -> Army
attacking attackers = min (attackers - 1) 3

defending :: Army -> Army
defending defenders = min defenders 2

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

