-- By Mathias Bothen ma6728bo-s
-- and Filip Bergman fi5731be-s

scoreSpace = -1
scoreMismatch = -1
scoreMatch = 0
string1 = "writers"
string2 = "vintner"

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

-- Part 3: Optimized Similarity score
optSimilarityScore :: String -> String -> Int
optSimilarityScore xs ys = tableVal (length xs) (length ys)
  where
    tableVal i j = mcsTable!!i!!j
    mcsTable = [[ tableEntry i j | j<-[0..]] | i<-[0..] ]
        
    tableEntry :: Int -> Int -> Int
    tableEntry 0 0 = 0
    tableEntry a 0 = scoreSpace * a
    tableEntry 0 b = scoreSpace * b
    tableEntry i j = l
      where
          l = maximum [(prev1 + score x y), 
                  (prev2 + score x '-'), 
                  (prev3 + score '-' y)]
          x = xs!!(i-1)
          y = ys!!(j-1)
          prev1 = tableVal (i-1) (j-1)
          prev2 = tableVal (i-1) j
          prev3 = tableVal i (j-1)


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

-- Part 3: Optimized optimal alignments
optoptAlignments :: String -> String -> [AlignmentType]
optoptAlignments xs ys = map (mapTuple reverse) (snd $ tableVal (length xs) (length ys))
  where
    tableVal i j = mcsTable!!i!!j
    mcsTable = [[ tableEntry i j | j<-[0..]] | i<-[0..] ]
       
    tableEntry :: Int -> Int -> (Int, [AlignmentType])
    tableEntry 0 0 = (0, [([], [])])
    tableEntry a 0 = (scoreSpace * a, [(take a xs, replicate a '-')])
    tableEntry 0 b = (scoreSpace * b, [(replicate b '-', take b ys)])
    tableEntry i j = (fst (head l), concat (map snd l))
      where
         l = (maximaBy fst [(prev1 + score x y, attachHeads x y (snd $ tableVal (i-1) (j-1))), 
                  (prev2 + score x '-', attachHeads x '-' (snd (tableVal (i-1) j))), 
                  (prev3 + score '-' y, attachHeads '-' y (snd $ tableVal i (j-1)))])
         x = xs!!(i-1)
         y = ys!!(j-1)
         prev1 = fst $ tableVal (i-1) (j-1)
         prev2 = fst $ tableVal (i-1) j
         prev3 = fst $ tableVal i (j-1)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- E
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do 
  let ali = (optoptAlignments string1 string2)
  putStrLn ("There are " ++ show (length ali) ++ " optimal alignments:") 
  outputHelper ali
  putStrLn ("There was " ++ show (length ali) ++ " optimal alignments...") 

outputHelper :: [AlignmentType] -> IO ()
outputHelper [] = putStrLn "\n"
outputHelper (x:xs) = do
  putStrLn ((fst x) ++ "\n" ++ (snd x) ++ "\n")
  outputHelper xs
