module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.List
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return (rulesApply (map (newFunc r) brain))

newFunc :: Float -> (Phrase, [Phrase]) -> PhrasePair
newFunc r pp = (fst pp, pick r (snd pp))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply x y
 | transformationsApply "*" id x y /= Nothing = (justToVal (transformationsApply "*" reflect x y))
 | otherwise = []

reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs)
 | elem x (map fst reflections) = ((map snd reflections) !! justToVal(elemIndex x (map fst reflections))) : (reflect xs)
 | otherwise = x : reflect xs

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile x = map otherFunc x

otherFunc :: (String, [String]) -> (Phrase, [Phrase])
otherFunc x = (words (map toLower (fst x)), map (words . (map toLower)) (snd x))

--------------------------------------

reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argumen
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute x (y:ys) z
  | x == y    = z ++ (substitute x ys z)
  | otherwise = y: (substitute x ys z)
{-  ++ Adds two lists together, which makes it possible to add 
    the list z with the recursive call which also gives a list // Filip & Mathias-}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing

match x (y:ys) (z:zs)
  | x == y && length ys == 0  = Just (z:zs) -- If the pattern IS the wildcard
  | x == y                    = orElse (singleWildcardMatch (y:ys) (z:zs)) (longerWildcardMatch (y:ys) (z:zs))
  | x /= y && y == z          = match x ys zs
  | y /= z                    = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch (wc:ps) (x:xs)
  | ps == xs   = Just [x]
  | otherwise  = Nothing

longerWildcardMatch _ [] = Nothing
longerWildcardMatch (wc:ps) (x:xs) 
  | length ps > length (x:xs)             = Nothing
  | head ps /= x                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  | elemIndex wc (tail ps) /= Nothing     = Just []
  | ps /= (x:xs)                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  | ps == (x:xs)                          = Just []
  | otherwise                             = Nothing

-- Helperfunction that concants an element to a maybe list
concatWithMaybe :: a -> Maybe [a] -> Maybe [a]
concatWithMaybe _ Nothing   = Nothing
concatWithMaybe x (Just xs) = Just (x : xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc func x (y, (z:zs))
  | match wc y x /= Nothing             = Just (substitute wc (z:zs) (func(justToVal(match wc y x))))
  | otherwise                           = Nothing

justToVal :: Maybe a -> a
justToVal (Just a) = a

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc func (x:xs) y
 | transformationApply wc func y x /= Nothing = transformationApply wc func y x
 | otherwise = transformationsApply wc func xs y


