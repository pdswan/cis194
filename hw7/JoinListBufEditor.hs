module Main where

import Buffer
import JoinListBuffer
import Editor

bufferFromString :: String -> EditorJoinList
bufferFromString = fromString

main = runEditor editor $ (bufferFromString . unlines)
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
