{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import JoinList
import Buffer
import Scrabble
import Sized

type Annotation = (Score, Size)
type EditorJoinList = JoinList (Score, Size) String

makeAnnotation :: String -> Annotation
makeAnnotation s = (scoreString s, Size 1)

makeSingle :: String -> EditorJoinList
makeSingle s = Single (makeAnnotation s) s

scoreOfEditorJoinList :: EditorJoinList -> Int
scoreOfEditorJoinList = getScore . fst . tag

sizeOfEditorJoinList :: EditorJoinList -> Int
sizeOfEditorJoinList = getSize . size . tag

instance Buffer EditorJoinList where
  toString = unlines . toList
  fromString = (foldl (push makeSingle) Empty) . lines
  line = indexJ
  replaceLine n l b =
    (takeJ n b) +++ (makeSingle l) +++ (dropJ (n + 1) b)
  numLines = sizeOfEditorJoinList
  value = scoreOfEditorJoinList

