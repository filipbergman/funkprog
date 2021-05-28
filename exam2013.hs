import Data.Char
import Data.List
import Data.Maybe

-- Q1
scanr' :: (a -> b -> b) -> b ->  [a] -> [b]
scanr' f a [] = [a]
scanr' f a xs = scanr' f (f (last xs) a) (init xs) ++ [a]

--scanr' f a xs = reverse [(foldr f a (drop y xs)) | y <- reverse [0..(length xs)]]

scanl'' :: (a -> b -> a) -> a ->  [b] -> [a]
--scanl'' f a [] = [a]
--scanl'' f a (x:xs) = [a] ++ scanl'' f (f a x) xs

scanl'' f a xs = [(foldl f a (take y xs)) | y <- [0..(length xs)]]

reverse' :: [a] -> [a]
reverse' xs = foldl (flip (:)) [] xs

-- Q2
-- The relation a subclass has to its parent is that it puts a constraint on what
-- elements of the class can be. For example the Num class is a subclass to the Eq
-- class. This means that for a type to be part of the Num class, it firstly have
-- to be part of the Eq class. The Num class can have more constraints on the types
-- that wants to be part of the Num class. 

-- Q3
--twoC `plusC` twoC: (f.f f) . (f.f f)
--fourC: succC . succC (twoC f) = succC . succC f.f = succC . (f.(f.f f)) =
--    succC . succC (twoC f) = succC . (f.(twoC f))

--f.((f.(twoC f)) f)

(twoC f) . (twoC f) = (f.f) . (twoC f) = f.(f.(twoC f))
    = f.(succC twoC f) = succC threeC

-- Q4
f :: (Monad m, Num b) => m a -> m a -> m a
[2, 4, 8, 4, 8, 16, 6, 12, 24]
Nothing
Yes, it will return Just 7
Just 5 :: (Monad m, Num a) => m a

-- Q5
