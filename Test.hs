module Main where
import Data.Char    


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

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerVals = quickSort [a | a <- xs, a <= x]
        biggerVals  = quickSort [a | a <- xs, a > x]
    in  smallerVals ++ [x] ++ biggerVals
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
