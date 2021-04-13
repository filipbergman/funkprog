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
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply (x:xs) y
 | transformationsApply "*" id (x:xs) y /= Nothing =  justToVal(transformationsApply "*" id (x:xs) (reflect y))
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
{- TO BE WRITTEN -}
rulesCompile _ = []


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

longerWildcardMatch (wc:ps) (x:xs) 
  | length ps > length (x:xs)             = Nothing
  | head ps /= x                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  | elemIndex wc (tail ps) /= Nothing     = Just []
  | ps /= (x:xs)                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  | ps == (x:xs)                          = Just []
  | otherwise                             = Nothing

{-- WORKINGlongerWildcardMatch (wc:ps) (x:xs) -- TODO: match '*' "* and * f" "you and me 123" does not work
  | length ps > length (x:xs)             = Nothing
  | head ps /= x                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  | elemIndex wc (tail ps) /= Nothing     = Just []
  | ps /= (x:xs)                          = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
  -- | elemIndex wc (tail ps) /= Nothing     = concatWithMaybe x (longerWildcardMatch (wc:(  drop (fromMaybe 0 (elemIndex wc (tail ps))) ps  )) xs) -- Maybe not necessary?
  | ps == (x:xs)                          = Just []
  | otherwise                             = Nothing --}

--OLDlongerWildcardMatch (wc:ps) (x:xs) 
--  | length ps > length (x:xs)        = Nothing ???
--  | ps /= (x:xs)                     = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
--  | head ps /= x                     = concatWithMaybe x (longerWildcardMatch (wc:ps) xs)
--  | ps == (x:xs) || head ps == wc    = Just []
--  | otherwise                        = Nothing

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
--transformationApply _ _ _ _ = Nothing
transformationApply wc func (x:xs) ((y:ys), (z:zs))
  | match wc (y:ys) (x:xs) /= Nothing   = Just (substitute wc (z:zs) (func(justToVal(match wc (y:ys) (x:xs)))))
  | otherwise                           = Nothing


justToVal :: Maybe a -> a
justToVal (Just a) = a


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc func (x:xs) (y:ys)
 | transformationApply wc func (y:ys) x /= Nothing = transformationApply wc func (y:ys) x
 | otherwise = transformationsApply wc func xs (y:ys)


