main :: IO ()
main = print (hanoi 5 "1" "2" "3")


--Exercise 1
toDigits :: Integer -> [Integer]
toDigits x = turnItAround(toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x =  (x `mod` 10): toDigitsRev(x `div` 10)

turnItAround :: [Integer] -> [Integer]
turnItAround [] = []
turnItAround (x:xs) = turnItAround xs ++ [x]

--Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:[]) = (2*x):y:[]
doubleEveryOther (x:y:xs) =
    if (listLen xs) `mod` 2 == 0 then (2*x):(y):doubleEveryOther xs
    else (x:(2*y):doubleEveryOther (xs))

listLen :: [Integer] -> Integer
listLen [] = 0
listLen (x:xs) = 1 + listLen xs

--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
    if (x `div` 10)>=1 then sumDigits(toDigits x) + sumDigits xs
    else x+sumDigits xs

--Exercise 4
modCheck :: Integer -> Bool
modCheck x = (x `mod` 10)==0

validate :: Integer -> Bool
validate x = modCheck(sumDigits(doubleEveryOther(toDigits x)))

--Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,c)]
hanoi x a b c = (hanoi (x-1) a c b) ++ [(a,c)] ++ (hanoi (x-1) b a c)