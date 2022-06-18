module Homework4 where
import Data.Maybe
import Data.Sequence (mapWithIndex)


fun1 :: [Integer] -> Integer
fun1 = product . map (+(-2)) . filter (even)

fun2 :: Integer -> Integer
fun2 = sum . takeWhile (>1) . iterate (\x -> if even x then (x `div` 2) else (3*x+1))

--Problem 2

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addToTreeUpdate Leaf

addToTreeUpdate :: a -> Tree a -> Tree a
addToTreeUpdate x tree = updateHeights (addToTree x tree)

addToTree :: a -> Tree a -> Tree a
addToTree x Leaf = Node 0 Leaf x Leaf
addToTree x tree@(Node currHeight left@Leaf val right)
    = Node currHeight (addToTree x Leaf) val right
addToTree x tree@(Node currHeight left@Node {} val right@Leaf)
    = Node currHeight left val (addToTree x Leaf)
addToTree x tree@(Node currHeight left@(Node leftSize _ _ _) val
                                right@(Node rightSize _ _ _))
    | rightSize<leftSize = Node currHeight left val (addToTree x right)
    | otherwise = Node currHeight (addToTree x left) val right

updateHeights :: Tree a -> Tree a
updateHeights = postOrderMap updateHeight

postOrderMap :: (Tree a -> Tree a) -> Tree a -> Tree a
postOrderMap fun tree@Leaf = fun Leaf
postOrderMap fun tree@(Node x left val right) = fun (Node x (postOrderMap fun left) val (postOrderMap fun right))

updateHeight :: Tree a -> Tree a
updateHeight tree@(Leaf) = Leaf
updateHeight tree@(Node _ left@(Leaf) val right@(Leaf))
    = Node 0 left val right
updateHeight tree@(Node _ left@(Leaf) val right@(Node rightH _ _ _))
    = Node (rightH + 1) Leaf val right
updateHeight tree@(Node _ left@(Node leftH _ _ _) val right@(Leaf))
    = Node (leftH + 1) left val right
updateHeight tree@(Node _ left@(Node leftH _ _ _) val right@(Node rightH _ _ _))
    | leftH<rightH = Node (rightH + 1) left val right
    | otherwise = Node (leftH + 1) left val right

--prob 3

xor :: [Bool] -> Bool
xor = foldr xor' False

xor' :: Bool -> Bool -> Bool
xor' x y = (not ( x && y)) && (x || y)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (mapFold f) []

mapFold :: (a -> b) -> a -> [b] -> [b]
mapFold f a as = (f a):as

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

--Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = boolListToPrimes (filterNonPrimes (createList (x+1))) 0

createList :: Integer -> [Bool]
createList x = replicate (fromIntegral x) True

filterNonPrimes :: [Bool] -> [Bool]
filterNonPrimes xs = last (takeWhile ([] /=) (iterateSucc filterNonPrimesI xs 1))

filterNonPrimesI :: [Bool] -> Integer -> [Bool]
filterNonPrimesI xs i = fromMaybe [] (fromMaybe Nothing (safeLast (takeWhile isJust (iterateSucc (changeIJ i) (Just xs) i))))

changeIJ :: Integer -> Maybe [Bool] ->  Integer -> Maybe [Bool]
changeIJ i bools j = modifyArrayAt (fromMaybe [] bools) False (fromIntegral (i+j+2*i*j))

modifyArrayAt :: [a] -> a -> Int -> Maybe [a]
modifyArrayAt xs x index
    | (length xs < (index+1)) || (0 > index) = Nothing
    | otherwise = Just (take index xs ++ (x:tail (drop index xs)))

-- isPrime :: Integer -> Integer -> Integer -> Bool 
-- isPrime k i j 

boolListToPrimes :: [Bool] -> Integer -> [Integer]
boolListToPrimes [] _ = []
boolListToPrimes (bool:xs) x
    | bool = (2*x+1):boolListToPrimes xs (x+1)
    | otherwise = boolListToPrimes xs (x+1)

-- filterNonPrimes :: [Integer] -> Integer -> Integer -> Integer -> [Integer]
-- filterNonPrimes xs k i j  = filter (isPrime k i j) xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing 
safeLast xs = Just (last xs)

iterateSucc :: (a -> Integer -> a) -> a -> Integer -> [a]
iterateSucc f x y = f x y:iterateSucc f (f x y) (succ y)

calcJ :: Integer -> Integer -> Maybe Integer
calcJ i x
    |1+(2*i)==0 = Nothing
    |(x-i)`mod`(1+(2*i))/=0 = Nothing
    |otherwise = Just ((x-i)`div`(1+(2*i)))

-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]





