module Main where

main = putStrLn "Hello, world!"

-- 2.1.1
maxi x y = 
    if x < y
        then return y
    else do 
        return x

-- 2.1.2
-- sumsq n returns 1*1 + 2*2 + ... + n*n
-- Recursively:
sumsq 1 = 1
sumsq n = n * n + sumsq (n-1)
-- Using mapping
sumsqm n = sum (map sq [1..n])
    where 
        sq x = x * x 
    
-- 2.1.3 The towers of Hanoi
hanoi 0 = 0
hanoi n = 1 + 2 * hanoi (n-1)
-- 1 + 2(1+2*(1+2*1))

-- 2.1.4
smallestFactor n = nextFactor 1 n
     
nextFactor k n
    | k >= n            = n
    | mod n (k+1) == 0  = k+1
    | otherwise         = nextFactor (k+1) n

numFactors n = nextFactor 1 n -- todo

-- 2.1.5
type Month = Integer
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y 
    | m < 0 || m > 12 = 0
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | mod y 4 == 0 = 29
    | otherwise = 28

data Date = Date Integer Month Integer

d1 = Date 2014 2 20

validDate :: Date -> Bool

validDate (Date y m d) 
    | (d>0) && (d <= daysInMonth m y)   = True
    | otherwise                         = False

-- 2.2.1
multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs

-- 2.2.2
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:ys)
    | y == z    = x: (substitute x y ys)
    | otherwise = z: (substitute x y ys)

