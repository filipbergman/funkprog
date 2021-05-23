module Main where
import Data.Char
import Data.List
import Data.Maybe

-- 2.1
data Proposition = Var Name -- Variable
                | Proposition :&: Proposition -- eller and
                | Proposition :|: Proposition -- eller or
                | Not Proposition -- eller not a proposition
    deriving(Eq, Show)

type Name = String

vars :: Proposition -> [Name]
vars (Var x)    = [x]
vars (a :&: b)  = vars a 'union' vars b
vars (a :|: b)  = vars a 'union' vars b
vars (Not a)    = vars a

truthValue :: Proposition -> [(Name, Bool)] -> Bool
truthValue (Var x) xs   = fromJust (lookup x xs)
truthValue (a :&: b) xs = truthValue xs a && truthValue xs b
truthValue (a :|: b) xs = truthValue xs a || truthValue xs b
truthValue (Not a) xs   = not $ truthValue xs a