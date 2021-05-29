import Data.Char
import Data.List
import Data.Maybe

-- Q1
f = flip $ (flip(/)) . (5-)
--g x y = map x (takeWhile (<=y) (iterate (4+) (3)))
--g x y = flip $ flip map (flip takeWhile (iterate (4+) (3)) ((<=) y)) x
--g x y = flip $ flip map $ flip takeWhile (iterate (4+) (3)) ((<=) y) x
g = flip $ flip map . flip takeWhile (iterate (4+) 3) . (>=)

-- Q2
-- par takes two arguments, and evaluates the first one to WHNF in parallel to returning the second one, if possible. It creates a spark, i.e. a possibility to create
-- a thread.
-- seq takes two arguments, and evaluates the first one to WHNF if possible then returns the second one.
-- pseq takes two arguments, and guarantees evaluation of the first one to WHNF and returns the second one.
-- par, seq, psek :: a -> b -> b

-- Q3
curry   :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry :: (d -> e -> f) -> (d, e) -> f
($)     :: (d -> e) -> d -> e
(:)     :: f -> [f] -> [f]
(.)     :: (v -> w) -> (u -> v) -> u -> w
-- 1.1: 
-- a = (d -> e)
-- b = d
-- c = e
-- uncurry ($) :: ((a -> b), a) -> b
-- This type takes a pair with a (function, value) element and returns the result of the function applied to that value.
-- 1.2: 
-- a = f
-- b = [f]
-- c = [f]
-- uncurry (:) :: (f, [f]) -> [f]
-- Takes a pair with a value and a list of values of the same type, and returns a list with that value inserted at the front.
-- 1.3:
-- a = (v -> w)
-- b = (u -> v)
-- c = u -> v
-- uncurry (.) :: ((b -> c),(a -> b)) -> a -> c
-- takes a pair of functions, a value, applies the value to the second function, then applies the result of that function on 
-- the first function, then return the result of that.
-- 1.4:
-- a = (d -> e -> f)
-- b = (d, e)
-- c = f
-- uncurry uncurry :: ((a -> b -> c),(a, b)) -> c
-- Takes a pair of a function and a pair, applies the function on the first, then the second value in the inner pair through 
-- currying, and returns the result of that.
-- 1.5:
-- (d,e) = ??? Type error
-- curry uncurry :: 

curry   :: ((d, e) -> f) -> d -> e -> f
uncurry :: (a -> b -> c) -> (a, b) -> c

-- Q4
fmap f [] = []
fmap f (x:xs) = f x : (fmap f xs)