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
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
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
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

type AlignmentType = (String,String)

-- D
-- Uncurry makes similarityScore take a tuple as argument instead of 2 strings
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry simScore) (concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y:ys)), attachHeads '-' y (optAlignments (x:xs) ys)])

simScore :: String -> String -> Int
simScore xs [] = scoreMismatch * length xs
simScore [] ys = scoreMismatch * length ys
simScore (x:xs) (y:ys) = simScore xs ys + score x y

--optoptAlignments :: String -> String -> [AlignmentType]
--optoptAlignments [] [] = [([], [])]
--optoptAlignments (x:xs) [] = attachHeads x '-' (optoptAlignments xs [])
--optoptAlignments [] (y:ys) = attachHeads '-' y (optoptAlignments [] ys)
--optoptAlignments (x:xs) (y:ys) = maximaBy (uncurry simScore) (concat [attachHeads x y (optoptAlignments xs ys), attachHeads x '-' (optoptAlignments xs (y:ys)), attachHeads '-' y (optoptAlignments (x:xs) ys)])


optoptAlignments :: String -> String -> [AlignmentType]
optoptAlignments xs ys = map (mapTuple reverse) (snd $ tableVal (length xs) (length ys))
  where
    tableVal i j = mcsTable!!i!!j
    mcsTable = [[ tableEntry i j | j<-[0..]] | i<-[0..] ]
       
    tableEntry :: Int -> Int -> (Int, [AlignmentType])
    tableEntry 0 0 = (0, [([], [])])
    tableEntry a 0 = (scoreSpace * a, [(take a xs, replicate a '-')])
    tableEntry 0 b = (scoreSpace * b, [(replicate b '-', take b ys)])
    tableEntry i j 
     | x == y = (prev1 + score x y, attachHeads x y (snd $ tableVal (i-1) (j-1)))
     | otherwise = head (maximaBy fst [(prev2 + score x '-', attachHeads x '-' (snd (tableVal (i-1) j))), (prev3 + score '-' y, attachHeads '-' y (snd $ tableVal i (j-1)))])
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
         prev1 = fst $ tableVal (i-1) (j-1)
         prev2 = fst $ tableVal (i-1) j
         prev3 = fst $ tableVal i (j-1)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do 
  let ali = (optAlignments string1 string2)
  putStrLn ("There are " ++ show (length ali) ++ " optimal alignments:") 
  outputHelper ali

outputHelper :: [AlignmentType] -> IO ()
outputHelper [] = putStrLn "\n"
outputHelper (x:xs) = do
  putStrLn ((fst x) ++ "\n" ++ (snd x) ++ "\n")
  outputHelper xs
