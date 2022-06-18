module Pancakes (
    pancakes
) where



pancakes :: [Int]->[Int]
pancakes [x] = []
pancakes xs = [(minimum xs),(length xs)] ++ (pancakes (take ((length xs) - 1) (flipToBack (minimum xs) xs)))

flipMe :: Int -> [Int] -> [Int]
flipMe n xs = (reverse (take n xs)) ++ drop n xs

flipToBack :: Int -> [Int] -> [Int]
flipToBack n xs = flipMe (length xs) (flipMe n xs)


