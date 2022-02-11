module Golf where


--Problem 1
skips :: [a] -> [[a]]
skips xs = skipsNRec (length xs) xs

skipsNRec :: int -> [a] -> [[a]]
skipsNRec 1 xs = [xs] 
skipsNRec n xs = (skipsNRec (n-1) xs) ++ (skipsN n xs)

skipsN :: int -> [a] -> [a]
skipsN n [] = []
skipsN n xs = (getN n xs) ++ skipsN n (drop n xs)

getN :: int -> [a] -> [a]
getN n [] = []
getN n x:xs = getN (n-1) xs
getN 0 x:xs = x




