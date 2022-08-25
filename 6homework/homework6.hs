{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Homework6 where

--exercise 1

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = Prelude.map fib [0..]

--exercise 2

fib2 :: [Integer] -> [Integer]
fib2 [x,y] = [y,x+y]
fib2 _ = []

fibs2 :: [Integer]
fibs2 = (++) [1] $ concatMap tail $ iterate fib2 [1,1]

--exercise 3

-- class Stream a where
--     cons :: a -> Stream a
--     streamToList :: Stream a -> [a]
--     streamToList foo@(cons x xs) = [x] ++ (streamToList xs)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList str@(Cons x strm) = x : streamToList strm

instance Show a => Show (Stream a) where
    show strm@(Cons x xs) = show x ++ "," ++ show xs

--Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f strm@(Cons now next) = Cons (f now) (streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

--Exercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0

streamZip :: Stream a -> Stream a -> Stream a
streamZip strmA@(Cons x xs) strmB = Cons x $ streamZip strmB xs

ruler :: Stream Integer
ruler = infZipSucc 0

infZipSucc :: Integer -> Stream Integer
infZipSucc x = streamZip (streamRepeat x) (infZipSucc (x+1))

--Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 (streamRepeat 0)

constant :: Integer -> Stream Integer 
constant x = Cons x $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0)
    negate a@(Cons x xs) = Cons (-x) $ negate xs
    (+) a@(Cons x xs) b@(Cons y ys) = Cons (x+y) $ (+) xs ys
    (*) a@(Cons x xs) b@(Cons y ys) = Cons (x*y) $ (streamMap (*x) ys)+(xs*b)

instance Fractional (Stream Integer) where
    (/) a@(Cons x xs) b@(Cons y ys) = Cons (x `div` y) $ (Cons (1`div`y) (streamRepeat 0))*(xs-((a/b)*(ys)))

fibs3 :: Stream Integer 
fibs3 = x / (1-x-x^2)

data Matrix = Mat2x2 Integer Integer Integer Integer