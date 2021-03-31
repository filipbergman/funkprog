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

-- 2.3.4
smallestFactor n = nextFactor 1 n
     
nextFactor k n
    | k >= n            = n
    | mod n (k+1) == 0  = k+1
    | otherwise         = nextFactor (k+1) n

numFactors n = nextFactor 1 n -- todo
