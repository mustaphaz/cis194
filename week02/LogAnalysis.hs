{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s
    | typeCode == "I" = LogMessage Info (getTimestamp w) (getLogText w)
    | typeCode == "W" = LogMessage Warning (getTimestamp w) (getLogText w)
    | typeCode == "E" = LogMessage (Error errorCode) (getTimestamp w) (getLogText w)
    | otherwise = Unknown s
    where
        w = words s
        typeCode = w!!0
        errorCode = read (w!!1) :: Int

parse :: String -> [LogMessage]
parse s = [ parseMessage l | l <- lines s]

getTimestamp :: [String] -> TimeStamp
getTimestamp w
    | w!!0 == "E" = read (w!!2) :: TimeStamp
    | otherwise = read (w!!1) :: TimeStamp

getLogText :: [String] -> String
getLogText w
    | w!!0 == "E" = unwords(drop 3 w)
    | otherwise =  unwords (drop 2 w)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ t1 _) t@(Node lt y@(LogMessage _ t2 _) rt)
    | t1 < t2 = Node (insert x lt) y rt
    | otherwise = Node lt y (insert x rt)
