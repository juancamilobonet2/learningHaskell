{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad.Loops
import Data.List (sort)
--import Control.Applicative (Applicative(liftA2))

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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle bat = do
  matches <- matchups
  foldl (flip ($)) (return bat) (map fight matches)
  where
    getDieThrows x max= fmap (reverse . sort) $ sequence $ replicate (min x max) die
    atkDie = getDieThrows (attackers bat - 1) 3
    defDie = getDieThrows (defenders bat) 2
    matchups = zip <$> atkDie <*> defDie

fight :: (DieValue, DieValue) -> Rand StdGen Battlefield -> Rand StdGen Battlefield
fight (dieAttack, dieDefend) mbat = do
  bat <- mbat
  if dieAttack>dieDefend then
    return $ bat {defenders = defenders bat - 1}
    else
      return $ bat {attackers = attackers bat - 1}

safeHead :: [a] -> Maybe a
safeHead xs
  | not (null xs) = Just $ head xs
  | otherwise = Nothing


-- Exercise 2

invade :: Battlefield -> Rand StdGen Battlefield
invade bat = iterateUntilM batFinished battle bat

batFinished :: Battlefield -> Bool
batFinished bat
  | attackers bat < 2 || defenders bat < 1 = True
  | otherwise = False

-- Exercise 3

successProb :: Battlefield -> Rand StdGen Double
successProb bat =
  fmap (\x -> fromIntegral x/1000)
  $ fmap (sumIf id)
  $ sequence
  $ map (fmap batWon)
  $ map invade
  $ replicate 1000 bat

sumIf :: (a -> Bool) -> [a] -> Int
sumIf fun xs = sum $ map condition xs
  where
    condition x = if fun x then 1 else 0

batWon :: Battlefield -> Bool
batWon bat
  | defenders bat < 1 = True
  | otherwise = False
