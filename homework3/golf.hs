module Golf(
    skips
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
