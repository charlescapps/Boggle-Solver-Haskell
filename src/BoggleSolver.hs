module BoggleSolver where
import Data.Array
import Data.List
import BoggleGame
import BoggleHash
import BoggleTrie

--represents a snakey path through the boggle matrix
type Play = ([(Int,Int)],String) --list of indices into matrix and the word

--Display a list of Play's. Trivial but useful.  
showPlays :: [Play] -> String
showPlays ps = unlines $ map show ps

-----------------CODE FOR NAIVE ALGORITHM USING HASH TABLE----------------------
--returns all valid plays (words in dict) at some index and some max length
getGoodPlaysAt :: BogHashTable -> BogGame -> (Int,Int) -> Int -> [Play]
getGoodPlaysAt ht bg ix len = 
    [ p | p <- getAllPlaysAt bg ix len , BoggleHash.inDict ht (snd p)]

--Take a boggle game, the current index, the remaining length, plays so far
--Returns all possible plays with (word size <= len)
--Recursively adds letters to each play provided there's an adjacent cube
--that hasn't been used yet.
getAllPlaysAt :: BogGame -> (Int, Int) -> Int -> [Play]
getAllPlaysAt bg ix len = 
    [ (reverse $ fst p, reverse $ snd p) | p <- reversedPlays ]
        where reversedPlays = getAllPlaysAt' bg ix len []

--Strat is to add current letter to the move we're building, then do a 
--recursive call for each adjacent cube.
getAllPlaysAt' :: BogGame -> (Int, Int) -> Int -> [Play] -> [Play]
getAllPlaysAt' (BogGame size g) (n,m) len plays 
    | len == 0 = []
    | concatPlays == [] = [] --happens when we've visited all adj. cubes already
    | otherwise = concatPlays ++ concat [ getAllPlaysAt' bg ix (len - 1) concatPlays | 
            ix <- newCubes]
        where diffs = [(-1,-1),(-1,0),(0,-1),(1,0),(0,1),(1,1),(-1,1),(1,-1)]
              newCubes = [ (i+n,j+m) | (i,j) <- diffs, 
                                 i+n >= 0, i+n < size, 
                                 j+m >= 0, j+m < size ]
              bg = (BogGame size g)
              concatPlays = newPlays bg plays (n,m)

--prepend current letter to the plays, and the current index
newPlays :: BogGame -> [Play] -> (Int, Int) -> [Play]
newPlays (BogGame n g) plays ix
    | plays == [] = [([ix], [g!ix])]
    | otherwise = [ ( ix:(fst p), (g!ix) : (snd p) ) | 
            p <- plays, not $ ix `elem` (fst p) ]
            
--Helper: filter out plays that already contain an index (Int,Int)
filterPlays :: (Int, Int) -> [Play] -> [Play]
filterPlays ix plays = [ p | p <- plays, not $ ix `elem` (fst p)]

-----------------GET ALL PLAYS AND SCORES, NAIVE ALGORITHM----------------------
--game, max len -> list of plays/scores 
solveGame :: BogGame -> BogHashTable -> Int -> [Play] 
solveGame (BogGame n board) ht maxLen =  
    [ play | i <- [0..n-1], j <- [0..n-1], play <- getGoodPlaysAt ht (BogGame n board) (i,j) maxLen ]

sortByLenThenAlpha :: [Play] -> [String]
sortByLenThenAlpha solns = sortBy compLen (sort words)
    where words = map snd solns

--remove duplicate words. 
listToSet :: (Eq a) => [a] -> [a] 
listToSet xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs

playsToSet :: [Play] -> [Play]
playsToSet xs = foldr (\x acc -> if snd x `elem` getWords acc then acc else x:acc) [] xs
    where getWords ws = map snd ws

--Put it together. Output list of Strings in proper order given board, max len
solveAndGetWords :: BogGame -> BogHashTable -> Int -> [String]
solveAndGetWords game ht maxLen = sortByLenThenAlpha (solveGame game ht maxLen)

solvePretty :: BogGame -> BogHashTable -> Int -> String
solvePretty game ht maxLen = (prettySolnsTot . groupByLen . listToSet) 
    (solveAndGetWords game ht maxLen)

--group by length
groupByLen :: [String] -> [[String]]
groupByLen [] = []
groupByLen words = first : groupByLen theRest
    where first = takeWhile (\x -> length x == (length $ head words)) words
          theRest = dropWhile (\x -> length x == (length $ head words)) words

--Pretty output
prettySolns :: [[String]] -> String
prettySolns [] = ""
prettySolns (g:gs) = (show . score . head) g ++ " Points:\n" 
                   ++ (unlines (map ("\t" ++) g))
                   ++ prettySolns gs

prettySolnsTot :: [[String]] -> String
prettySolnsTot gs = prettySolns gs ++ "\nTOTAL POINTS = " ++ show total
    where total = sum $ concat $ (map . map) score gs

--equivalent to sorting by length, meh
compLen :: String -> String -> Ordering
compLen w1 w2 | length w1 < length w2 = LT
              | length w1 == length w2 = EQ
              | otherwise = GT
