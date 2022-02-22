module Golf(
    skips,
    localMaxima,
    histogram
) where
--Problem 1

skips :: [a] -> [[a]]
skips xs = skipsNRec (length xs) xs

skipsNRec :: Int -> [a] -> [[a]]
skipsNRec 0 xs = []
skipsNRec 1 xs = [xs]
skipsNRec n xs = skipsNRec (n-1) xs++[skipsN n (drop (n-1) xs)]

skipsN :: Int -> [a] -> [a]
skipsN n [] = []
skipsN n (x:xs) = x : skipsN n (drop n (x:xs))

-- skips takes the list and calls the recursive funciton skipsNRec, giving it
-- the list length as the parameter n. skipsNRec then calls skipsN, which transforms
-- the input list into a list with only every n'th element. Then skipsNRec is called 
-- again but this time with n-1 as the element to be extracted. 

--Exercise 2: Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:xs) = maxSearch False x xs

maxSearch :: Bool -> Integer -> [Integer] -> [Integer]
maxSearch _ before [] = []
maxSearch more before (x:xs)
    | more && (before>x) = before : maxSearch False x xs
    | before<x = maxSearch True x xs
    | otherwise = maxSearch False x xs

--Local maxima calls the recursive function maxSearch, which takes three parameters. 
--The second parameter is the number being evalueted, the first parameter is a
--Boolean which says whether or not the number being evalueated is bigger than the
--number before it. The third parameter is the list. The number is compared to the number
--After it and it is determined whether or not it passes the test.

--Exercise 3: Histogram

histogram :: [Integer] -> String
histogram xs = makeLines (counter xs 9) ++ "==========\n0123456789"

makeLines :: [Integer] -> String
makeLines xs
    | (maximum xs) == 0 = ""
    | otherwise = makeLine (map (+(-1)) xs) ++ makeLine xs

makeLine :: [Integer] -> String
makeLine [] = "\n"
makeLine (x:xs)
    | x>0 = "*" ++ makeLine xs
    | x<=0 = " " ++ makeLine xs

counter :: [Integer] -> Integer -> [Integer]
counter xs (-1) = []
counter xs n = counter xs (n-1) ++ [countN xs n 0]

countN :: [Integer] -> Integer -> Integer -> Integer
countN [] _ count = count
countN (x:xs) n count
    | x==n = countN xs n (count+1)
    | otherwise = countN xs n count