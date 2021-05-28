import Data.Char
import Data.List
import Data.Maybe

-- Q1
-- f = curry $ (3.0+) . (uncurry $ flip (/))

-- g x y = map y (take x (iterate (+1) 1))
-- g x y = flip map (flip take (iterate (+1) 1) x) y
g = flip map . flip take (iterate (+1) 1)

-- Q2
{-
(:) :: d -> [d] -> [d]
(.) :: (b -> c) -> (a -> b) -> a -> c

-- a)
b = d
c = [d] -> [d]
(.)(:) :: (a -> d) -> a -> [d] -> [d]
-- b)
-- (:(.)) : is written as prefix but then it would need a parenthesis. 

-- c)
d = (b -> c) -> (a -> b) -> a -> c
((.):) :: [(b -> c) -> (a -> b) -> a -> c] -> [(b -> c) -> (a -> b) -> a -> c]

-- d)
d = d -> [d] -> [d]
((:):) :: [d -> [d] -> [d]] -> [d -> [d] -> [d]]

-- e)
(.) :: (b2 -> c2) -> (a2 -> b2) -> a2 -> c2
(.) :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
b2 = (b1 -> c1)
c2 = (a1 -> b1) -> a1 -> c1
(.)(.) :: (a2 -> b -> c) -> a2 -> (a1 -> b) -> a1 -> c
-}
-- Q3
-- A spark is a potential to create a thread. 
-- par allows evaluation of the first argument up to WHNF in parallel with returning the second argument
-- pseq forces evaluation of the first argument and returns the second argument.
-- seq allows for evaluation of the first argument up to WHNF if it is possible for the compiler
-- and return the second argument. 

-- Q4
--fmap' :: Functor f => (a -> b) -> f a -> f b
--fmap' f m = do  x <- m 
--                ; return (f x)

--fmap' f m = (m >>= (\x -> return $ f x)) 
fmap' f m = (m >>= (return . f))

-- Q5
-- They are equivalent because both will set the x value to 2000 for all elements in the lists, and the y element
-- will just be 1, 3, 5 and so on up to 99 in both cases. Computationally they may not be equivalent.

-- Q6
data Tree = Leaf String | Node Tree String Tree

subTree :: Tree -> Tree -> Bool
subTree (Leaf s) t2 = inTree s t2
subTree t1 (Leaf s) = False
subTree t1@(Node t11 s1 t12) t2@(Node t21 s2 t22) = 
    ((s1 == s2) && (subTree t11 t2) && (subTree t12 t2)) ||
    subTree t1 t21 ||
    subTree t1 t22

inTree :: String -> Tree -> Bool
inTree s (Leaf l) = s == l
inTree s1 (Node t1 s2 t2) = (s1 == s2) || (inTree s1 t1) || (inTree s1 t2)

tr1 = Node (Node (Leaf "1") "12" (Leaf "13")) "4" (Leaf "2") 
tr2 = Node (Leaf "1") "12" (Leaf "13")