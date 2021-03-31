module Main where

main = putStrLn "Hello, world!"
r = {- hello -} 5.0   -- x is 5.
area r = pi * r ^ 2
{-- long one

test
--} 

triple x = x^3
add x y = x + y
areaRect l w = l * w
areaSquare s = areaRect s s

heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where
    s = (a + b + c) / 2 -- Local variable

greeting :: IO()
greeting = do
    putStrLn "Enter your name"
    name <- getLine
    putStrLn("You " ++ name ++ ", me Haskell!")

q = [1,3..10]