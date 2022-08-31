{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Data.Time.Format.ISO8601 (yearFormat)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


--Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

maybeFirst :: (a -> b) -> Maybe (a,c) -> Maybe (b,c)
maybeFirst f perhaps
    | isNothing perhaps = Nothing
    | otherwise = Just $ first f (fromJust perhaps)

instance Functor Parser where
  fmap f x = Parser y
    where
      y = maybeFirst f . runParser x

--Exercise 2

applyParser :: Parser (a->b) -> Parser a -> String -> Maybe (b,String)
applyParser p1 p2 str
  | isNothing extractP1 = Nothing
  | isNothing extractP2 = Nothing
  | otherwise = Just (one, two)
  where
    extractP1 = runParser p1 str
    extractP2 = runParser p2 $ snd $ fromJust extractP1
    one = fst (fromJust extractP1) (fst (fromJust extractP2))
    two = snd $ fromJust extractP2



instance Applicative Parser where
  pure x = Parser y
    where
      y b = Just (x,b)
-- Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser (applyParser p1 p2)

--Exercise 3
abParser :: Parser (Char, Char)
abParser = makePair <$> char 'a' <*> char 'b'

-- (Char -> (Char -> (Char, Char))) -> Parser Char -> Parser (Char -> (Char,Char))
makePair :: a -> b -> (a, b)
makePair x y = (x,y)
-- Parser (Char -> (Char, Char)) -> Parser Char -> Parser (Char,Char)


abParser_ :: Parser ()
abParser_ = makePair_ <$> char 'a' <*> char 'b'

makePair_ :: a -> b -> ()
makePair_ x y = ()

intPair :: Parser [Integer]
intPair = makeIntList <$> posInt <*> char ' ' <*> posInt

makeIntList :: Integer -> a -> Integer -> [Integer]
makeIntList x y z  = [x,z]

--Exercise 4

failParser :: Parser a
failParser = Parser y
  where
    y x = Nothing

instance Alternative Parser where
  empty = failParser
  (<|>) p1 p2 = Parser fun
    where
      fun x
        | isNothing (runParser p1 x) = runParser p2 x
        | otherwise = runParser p1 x

--Exercise 5

emptyParser :: a -> ()
emptyParser x = ()

intOrUppercase :: Parser ()
intOrUppercase = emptyParser <$> posInt <|> (emptyParser <$> satisfy isUpper)