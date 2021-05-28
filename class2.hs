
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
vars (a :&: b)  = vars a `union` vars b
vars (a :|: b)  = vars a `union` vars b
vars (Not a)    = vars a

truthValue :: Proposition -> [(Name, Bool)] -> Bool
truthValue (Var x) xs   = fromJust (lookup x xs)
truthValue (a :&: b) xs = truthValue a xs && truthValue b xs
truthValue (a :|: b) xs = truthValue a xs || truthValue b xs
truthValue (Not a) xs   = not $ truthValue a xs

allVals :: [Name] -> [[(Name, Bool)]]
allVals [] = [[]]
allVals (x:xs) = [(x, b):val | val <- allVals xs, b <- [True, False]]

tautology :: Proposition -> Bool
tautology p = and [truthValue p val | val <- allVals (vars p)] -- and checks if all values in the list is True

hamlet :: Proposition
hamlet = Var "to be" :|: Not (Var "to be")

data File = Dir String [File] | File String
            deriving(Show, Eq)

type FileSystem = [File]

findFile :: FileSystem -> String -> [String]
findFile files name = [name | File f <- files, name == f] ++
                      [dir ++ "/" ++ path | Dir dir files' <- files, path <- findFile files' name]

exampleFileSystem :: FileSystem
exampleFileSystem =
  [ File "apa"
  , Dir "bepa" [ File "apa", Dir "bepa" [], Dir "cepa" [ File "bepa" ] ]
  , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ]
  ]

type Set a = [a]

createEmptySet :: Set a
createEmptySet = Set []

elemInSet :: (Eq a) => a -> Set a -> Bool
elemInSet elem [] = False
elemInSet elem (x:xs)
    | elem == x = True
    | otherwise = elemInSet elem xs

addElem :: (Eq a) => a -> Set a -> Set a
addElem value set
    | elemInSet value set = set
    | otherwise           = set ++ [value]

removeElem :: (Eq a) => a -> Set a -> Set a
removeElem value set = [a | a <- set, a /= value]

setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion xs ys = [a | a <- xs, elemInSet a ys]

addElemSorted :: (Ord a) => a -> Set a -> Set a
addElemSorted _ [] = []
addElemSorted value (Set set) = 
    let
        smallerElems = addElemSorted (head xs) (tail xs)
        biggerElems = addElemSorted (head ys) (tail ys)
    in 
        smallerElems ++ [value] ++ biggerElems
    where 
        xs = [a | a <- set, a < value]
        ys = [b | b <- set, b > value]

instance (Ord a, Ord b) => Ord (a,b) where
    (x,y) < (z,w)   = x < z || (x == z && y < w)
    (x,y) <= (z,w)  = x < z || (x == z && y <= w)
    (x,y) >= (z,w)  = x > z || (x == z && y >= w)
    (x,y) > (z,w)   = x > z || (x == z && y > w)
    max (x,y) (z,w) 
        | (x,y) > (z,w) = (x,y)
        | otherwise     = (z,w) 
    min (x,y) (z,w) 
        | (x,y) < (z,w) = (x,y)
        | otherwise     = (z,w) 

instance Ord b => Ord [b] where
    xs < ys  = 
    (<), (<=), (>=), (>) :: a -> a -> Bool 
    max, min             :: a -> a -> a