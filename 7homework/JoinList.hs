{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Data.Time.Format.ISO8601 (yearFormat)

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1@Empty jl2 = jl2
(+++) jl1 jl2@Empty = jl1
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2


tag :: Monoid m => JoinList m a -> m
tag jl@Empty = mempty
tag jl@(Single m _) = m
tag jl@(Append m _ _) = m

-- getValue :: Monoid m => JoinList m a -> Maybe a
-- getValue jl@Empty = Nothing 
-- getValue jl@(Single _ x) = Just x
-- getValue jl@()

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ jl@Empty = Nothing
indexJ _ jl@(Single _ x) = Just x
indexJ x jl@(Append m left right)
    | x>=getSize (size m) || x<0 = Nothing
    | x<leftSize = indexJ x left
    | otherwise = indexJ (x-leftSize) right
    where
        leftSize = getSize (size (tag left))

testSizedTree :: JoinList Size String
testSizedTree = (+++) ((+++) (Single (Size 1) "a")  (Single (Size 1) "b")) ((+++) (Single 1 "x") ((+++) (Single (Size 1) "c")  (Single (Size 1) "d")))

test2 :: JoinList Size String
test2 = (+++) Empty Empty

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ x jl@(Single _ _)
    | x>0 = Empty
    | otherwise = jl
dropJ x jl@(Append s left right)
    | x>=getSize (size s) = Empty
    | x>=leftSize = dropJ (x-leftSize) right
    | otherwise = (+++) (dropJ (leftSize-x) left) right
    where
        leftSize = getSize (size (tag left))


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 jl = Empty
takeJ x jl@(Single _ _)
    | x<=0 = Empty
    | otherwise = jl
takeJ x jl@(Append s left right)
    | x<=0 = Empty
    | x>=getSize (size s) = jl
    | x>leftSize = (+++) (left) (takeJ (x-leftSize) right)
    | x<=leftSize = takeJ x left
    where
        leftSize = getSize $ size $ tag left


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

scoreWord :: String -> JoinList (Score, Size) String 
scoreWord str = Single (scoreString str, Size 1) str

appendPairs :: Monoid m => [JoinList m a] -> [JoinList m a]
appendPairs [] = [Empty]
appendPairs [x] = [x]
appendPairs (x:y:xs) = (x +++ y) : appendPairs xs

instance Buffer (JoinList (Score, Size) String) where
  toString jl@Empty = ""
  toString jl@(Single _ x) = x
  toString jl@(Append _ x y) = toString x ++ " " ++ toString y

  fromString = head . head 
                    . dropWhile (\x -> (length x) > 1) 
                    . iterate appendPairs 
                    . map (scoreWord) 
                    . words


  line x = indexJ x  
  replaceLine x str jl = left +++ scoreWord str +++ right
                        where
                            left = takeJ (x-1) jl
                            right = dropJ x jl

  numLines x = getSize $ snd y
                where y = tag x
  value x = getScore $ fst y
                where y = tag x

