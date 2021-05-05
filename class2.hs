module Main where
import Data.Char
import Data.List
import Data.Maybe



main = putStrLn "Hello, world!"

rever :: [a] -> [a] 
rever = foldl (flip (:)) []