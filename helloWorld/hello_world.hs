main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)