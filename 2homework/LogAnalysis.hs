-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- Exercise 1

logMaker :: [String] -> LogMessage
logMaker xs
  | (head xs) == "E" = LogMessage (Error (read (xs !! 1) :: Int)) (read (xs !! 2) :: Int) (unwords (drop 3 xs))
  | (head xs) == "I" = LogMessage (Info) (read (xs !! 1) :: Int) (unwords (drop 2 xs))
  | (head xs) == "W" = LogMessage (Warning) (read (xs !! 1) :: Int) (unwords (drop 2 xs))
  | otherwise = Unknown (concat (xs))


parseMessage :: String -> LogMessage
parseMessage x = logMaker (words x)

parse :: String -> [LogMessage]
parse text = inOrder (build (logsLines (lines text)))

logsLines :: [String] -> [LogMessage]
logsLines [] = []
logsLines (x:xs) = parseMessage x : (logsLines xs)

--Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert logM@(LogMessage _ time _) m@(Node leftTree currentLog@(LogMessage _ currentTime _) rightTree) 
  | (time<currentTime) = Node (insert logM leftTree) currentLog rightTree 
  | (time>=currentTime) = Node leftTree currentLog (insert logM rightTree) 
insert logM m@(Leaf) = Node Leaf logM Leaf
insert logM@(Unknown _) msgTree = msgTree

--Exercise 3
build :: [LogMessage] -> MessageTree
build logs = builder logs (Leaf)

builder :: [LogMessage] -> MessageTree -> MessageTree
builder [] tree = tree
builder (log:logs) tree = builder logs (insert log tree)

--Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder tree@(Node leftTree centerLog rightTree) = (inOrder leftTree) ++ [centerLog] ++ (inOrder rightTree)
inOrder tree@Leaf = []

--Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = textList (inOrder (build (severityFilter logs))) 

severityFilter :: [LogMessage] -> [LogMessage]
severityFilter (log@(LogMessage (Error x) _ _) : logs)
  | (x>50) = [log] ++ severityFilter logs
  | (x<=50) = severityFilter logs
severityFilter (log : logs) = severityFilter logs
severityFilter [] = []

textList :: [LogMessage] -> [String]
textList (log@(LogMessage _ _ string) : logs) = [string] ++ textList logs
textList (log@(Unknown string) : logs) = [string] ++ textList logs
textList [] = [] 

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file