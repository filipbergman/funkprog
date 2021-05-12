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

-- Picks random phrase pairs and checks if rules apply.
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return $ rulesApply $ map (randomPhrasePair r) brain

-- Helper function that returns a PhrasePair with the first element in the BotBrain element and a random phrase 
-- from the second element.
randomPhrasePair :: Float -> (Phrase, [Phrase]) -> PhrasePair
randomPhrasePair r pp = (fst pp, pick r $ snd pp)

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply x y 
 | y == z = []
 | otherwise = z
 where z = try (transformationsApply "*" reflect x) y

-- Uses a helper function which we would have prefered to avoid. For every words, check if it is reflectable-
-- and if it is, reflect it. 
reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs)
 | elem x $ map fst reflections = (map snd reflections) !! justToVal(elemIndex x (map fst reflections)) : reflect xs
 | otherwise = x : reflect xs

-- Gives the value from a Maybe(Does not check for nothing)
justToVal :: Maybe a -> a
justToVal (Just a) = a

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
rulesCompile = map stringToPhrase

-- Helper function that converst the string-stringlist pair to a phrase-phraselist pair of lowercase characters. 
stringToPhrase :: (String, [String]) -> (Phrase, [Phrase])
stringToPhrase x = (words (map toLower (fst x)), map (words . (map toLower)) (snd x))

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

-- Fix allows us to repeat the same transformation over and over again until transformationsapply no longer-
-- change anything.
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply x y = fix (try (transformationsApply "*" id x)) y

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

{-  ++ Adds two lists together, which makes it possible to add 
    the list z with the recursive call which also gives a list -}
-- Replaces a wildcard in a list with the list given as the third argumen
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute x (y:ys) z
  | x == y    = z ++ (substitute x ys z)
  | otherwise = y: (substitute x ys z)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- Loops through the characters until the first character(y) is the same as the wildcard, then check-
-- with the helper functions. We realised afterwards that this is a bit backwards to how it is-
-- supposed to be, but it works.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (y:ys) (z:zs)
  | wc == y && length ys == 0  = Just (z:zs) -- If the pattern IS the wildcard
  | wc == y                    = orElse (singleWildcardMatch (y:ys) (z:zs)) (longerWildcardMatch (y:ys) (z:zs))
  | wc /= y && y == z          = match wc ys zs
  | y /= z                     = Nothing


-- Helper functions to match
-- Simply return if the rest of the arrrays are the same.
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
  | ps == xs   = Just [x]
  | otherwise  = Nothing

-- Recursive function that appends the characters to the maybe until the rest of the lists match.
longerWildcardMatch _ [] = Nothing
longerWildcardMatch (wc:ps) (x:xs) 
  | length ps > length (x:xs)             = Nothing
  | head ps /= x                          = mmap (x:) $ longerWildcardMatch (wc:ps) xs
  | elemIndex wc (tail ps) /= Nothing     = Just []
  | ps /= (x:xs)                          = mmap (x:) $ longerWildcardMatch (wc:ps) xs
  | ps == (x:xs)                          = Just []
  | otherwise                             = Nothing

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
transformationApply wc func x (y, z)
  | match wc y x /= Nothing             = Just $ substitute wc z $ func $ try (match wc y) x
  | otherwise                           = Nothing



-- Applying a list of patterns until one succeeds
-- Loops through all the Phrase pairs and compares with the given phrase to see what to replace.
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc func (x:xs) y
 | transformationApply wc func y x /= Nothing = transformationApply wc func y x
 | otherwise = transformationsApply wc func xs y