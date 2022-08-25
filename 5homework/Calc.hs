{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM (StackExp (PushI, Add, Mul), Program, stackVM, StackVal)
import Data.Maybe (fromMaybe, isNothing)


eval :: ExprT -> Integer
eval exp@(ExprT.Lit x) = x
eval exp@(ExprT.Add x y) = (eval x) + (eval y)
eval exp@(ExprT.Mul x y) = (eval x)*(eval y)


evalStr :: String -> Maybe Integer
evalStr str
    | isNothing (parseExp Lit ExprT.Add ExprT.Mul str) = Nothing
    | otherwise = Just (eval (fromMaybe (Lit 0) (parseExp Lit ExprT.Add ExprT.Mul str)))


class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y

instance Expr Integer where
    lit x = x
    add x y = x+y
    mul x y = x*y

instance Expr Bool where
    lit x
        | x<=0 = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr MinMax where
    lit x = MinMax x
    add a@(MinMax x) b@(MinMax y)
        | x>=y = a
        | x<y = b
    mul a@(MinMax x) b@(MinMax y)
        | x>=y = b
        | x<y = a

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add a@(Mod7 x) b@(Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul a@(Mod7 x) b@(Mod7 y) = Mod7 ((x*y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


--StackVM

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul ]


compile :: String -> Maybe Program
compile x = parseExp lit add mul x :: Maybe Program

runStackVM :: Maybe Program -> Either String StackVal
runStackVM x = stackVM $ fromMaybe [] x

