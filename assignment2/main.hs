scoreSpace = -1
scoreMismatch = -1
scoreMatch = 0
string1 = "writers"
string2 = "vintner"

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

-- A
similarityScore :: String -> String -> Int
similarityScore _ [] = scoreMismatch
similarityScore [] _ = scoreMismatch
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y, similarityScore xs (y:ys) + score x '-', similarityScore (x:xs) ys + score '-' y]

score :: Char -> Char -> Int
score x '-'  = scoreSpace
score '-' y  = scoreSpace
score x y
 | x == y = scoreMatch
 | otherwise = scoreMismatch


-- EXPLAIN WHAT THIS DOES
-- Attach h1 and h2 to the beginning of xs and ys which are lists drawn from the variable aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- C
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x >= maximum (map valueFcn xs)]

type AlignmentType = (String,String)

-- D
-- Uncurry makes similarityScore take a tuple as argument instead of 2 strings
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = []
optAlignments x [] = optAlignments x ['-']
optAlignments [] y = optAlignments ['-'] y
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) (concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y:ys)), attachHeads '-' y (optAlignments (x:xs) ys)])
--optAlignments (x:xs) (y:ys) = maximaBy (similarityScore ) []