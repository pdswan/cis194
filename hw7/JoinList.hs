module JoinList where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Show, Eq)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ b = b
a +++ Empty = a
a +++ b = Append (tag a `mappend` tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty   = Nothing
indexJ i (Single _ a)
  | i == 0       = Just a
  | otherwise    = Nothing
indexJ i (Append m left right)
  | i < leftSize = indexJ i left
  | otherwise    = indexJ (i - leftSize) right
  where leftSize = getSize . size $ tag left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 a = a
dropJ _ Empty = Empty
dropJ _ (Single b a) = Empty
dropJ i (Append m left right)
  | i < leftSize = (dropJ i left) +++ right
  | otherwise    = dropJ (i - leftSize) right
  where leftSize = getSize . size $ tag left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty               = Empty
takeJ 0 _                   = Empty
takeJ _ single@(Single b a) = single
takeJ i (Append m left right)
  | i < leftSize = (takeJ i left)
  | otherwise = left +++ (takeJ (i - leftSize) right)
  where leftSize = getSize . size $ tag left

push :: Monoid b => (a -> JoinList b a) -> JoinList b a -> a -> JoinList b a
push makeSingle list a = list +++ makeSingle a

toList :: JoinList a b -> [b]
toList Empty = []
toList (Single _ b) = [b]
toList (Append _ left right) = (toList left) ++ (toList right)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

