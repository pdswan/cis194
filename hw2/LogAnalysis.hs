{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

parseMessage :: String -> LogMessage
parseMessage message =
  case parsedMessageType of
    Just (messageType, x:xs) -> LogMessage messageType (read x) (unwords xs)
    _                        -> Unknown message
  where
    parsedMessageType = parseMessageType $ words message

    parseMessageType :: [String] -> Maybe (MessageType, [String])
    parseMessageType ("E":code:xs) = Just $ (Error (read code), xs) 
    parseMessageType ("I":xs) = Just $ (Info, xs)
    parseMessageType ("W":xs) = Just $ (Warning, xs)
    parseMessageType _ = Nothing

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree            = messageTree
insert messageToInsert Leaf               = (Node Leaf messageToInsert Leaf)
insert messageToInsert (Node l message r)
  | (ts messageToInsert) > (ts message)   = Node l message (insert messageToInsert r)
  | otherwise                             = Node (insert messageToInsert l) message r
  where
    ts :: LogMessage -> TimeStamp
    ts (LogMessage _ t _) = t
    ts (Unknown _)        = undefined

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map str . inOrder . build . relevantLogMessages
  where
    str :: LogMessage -> String
    str (LogMessage _ _ s) = s
    str (Unknown _)        = undefined

    relevantLogMessages :: [LogMessage] -> [LogMessage]
    relevantLogMessages = filter isRelevant

    isRelevant :: LogMessage -> Bool
    isRelevant (LogMessage (Error code) _ _)
      | code >= 50 = True
      | otherwise  = False
    isRelevant _   = False

