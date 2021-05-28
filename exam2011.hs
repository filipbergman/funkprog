import Data.Char
import Data.List
import Data.Maybe

{- Q1
(++) :: [a] -> [a] -> [a]
const :: a -> b -> a
map :: (a -> b) -> [a] -> [b]

map (const (++)) :: [b] -> [[a] -> [a] -> [a]]
const (++) :: b -> ([a] -> [a] -> [a])



const (map (++)) :: b -> [[a]] -> [[a] -> [a]]
a = [[a]] -> [[a] -> [a]]
b = b

map (++) :: [[a]] -> [[a] -> [a]]
a = [a]
b = [a] -> [a]
-}

-- Q2
-- Currying is when you take a function that takes a number of parameters and you bind them all together
-- to only one parameter in a tuple, this makes passing functions as parameters easier, since you can decide how
-- many parameters a function should take. Uncurrying is the opposite, you take a function that takes a tuple
-- and make it take the parameters of that tuple as individual parameters. Higher order functions become easier.

-- Q3
-- String, and the value is "currycurrycurrycurry". This is the result since the do notation takes a parameter
-- and an output after each ; since it uses bind in the background. In this case we put curry for each element
-- in the list.
-- [String], and the value is ["uncurry", "uncurry", "uncurry", "uncurry"]
-- Only difference between this and the previous one is that the strings are put in lists since return is called.

-- Q4
c a b = (a \\ (a \\ b)) -- \\ takes 2 lists a and b, then it firstly applies \\ to a and b. This means that it 
-- removes one instance of the elements from a that are in b. And then does that again.
-- c = removeElemsTwice
-- as \\ bs = foldl (flip delete) as bs
{-
flip delete :: [a] -> a -> [a] -- Man måste flippa annars passar inte mönstret a -> b -> a ([a] -> a -> [a])
-- (Första och sista elementen måste vara samma, om man inte flippar blir dom olika(a och [a]))
foldl (flip delete) :: [a] -> [a] -> [a]
(\\) :: [a] -> [a] -> [a] 
foldl :: (a -> b -> a) -> a -> [b] -> a -- (a -> b -> a) = [a] -> a -> [a] => a = [a], b = [a]
delete :: a -> [a] -> [a]
flip :: (a -> b -> c) -> b -> a -> c

-}

-- Q5
f = (/) . (5+)
g = ($) -- since g x y :: (a -> b) -> a -> b which is the same as ($)'

-- Q6
data Tree = Leaf Integer | Node Tree Integer Tree 

subTree :: Tree -> Tree -> Bool
subTree (Leaf i) t3 = inTree i t3
subTree (Node t1 x t2) (Leaf i) = False
subTree (Node t1 x t2) (Node t3 y t4) = 
    ((x == y) && (subTree t1 t3) && (subTree t2 t4)) ||
    subTree (Node t1 x t2) t3 ||
    subTree (Node t1 x t2) t4

inTree :: Integer -> Tree -> Bool
inTree i (Leaf j) = (i == j)
inTree x (Node t1 y t2) = (x == y) || inTree x t1 || inTree x t2 
    
tr1 = Node (Node (Leaf 1) 12 (Leaf 13)) 4 (Leaf 2) 
tr2 = Node (Leaf 1) 12 (Leaf 13)