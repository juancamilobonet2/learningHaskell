module Party where

import Employee
import Data.Tree

--Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp gl@(GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Semigroup GuestList where
    (<>) gl1@(GL emps1 fun1) gl2@(GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1<fun2 = gl2
    | otherwise = gl1

--Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tr = f value subF
    where
        subF = map (treeFold f) (subForest tr)
        value = rootLabel tr

--Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glp = (glCons boss (makeList right), makeList left)
    where 
        unzipped = unzip glp
        left = fst unzipped
        right = snd unzipped
        makeList = foldr (<>) mempty

--Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel $ tree

testComp :: Tree Employee
testComp = Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]

--Exercise 5
main = do
    treeString <- readFile "company.txt"
    contents <- readIO treeString
    let myGuests = maxFun contents
    putStrLn (show myGuests)
