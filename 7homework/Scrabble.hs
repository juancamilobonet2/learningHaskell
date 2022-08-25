{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char (toLower)
import GhcPrelude (Foldable(foldl'))

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)


instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = 0

score :: Char -> Score
score x
    | isLetter "aeilnorstu" = Score 1
    | isLetter "dg" = Score 2
    | isLetter "bcmp" = Score 3
    | isLetter "fhvwy" = Score 4
    | isLetter "k" = Score 5
    | isLetter "jx" = Score 8
    | isLetter "qz" = Score 10
    | otherwise = Score 0
    where
        isLetter = isInList (toLower x)

scoreString :: String -> Score
scoreString = foldr ((<>) . score) (Score 0)


isInList :: (Eq a) => a -> [a] -> Bool
isInList _ [] = False
isInList elem (x:xs)
    | elem == x = True
    | otherwise = isInList elem xs

getScore :: Score -> Int 
getScore (Score i) = i
