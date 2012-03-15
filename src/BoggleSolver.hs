module BoggleSolver where
import Data.List
import BoggleGame
import BoggleHash
import BoggleTrie
import Data.Array
import Data.Maybe

--represents a snakey path through the boggle matrix
type Play = ([(Int,Int)],String) --list of indices into matrix and the word

nilPlay :: Play
nilPlay = ([],"")

showPlayPretty :: Play -> String
showPlayPretty p = (snd p) ++ ":  " ++ (show $ fst p) 

--Display a list of Play's. Trivial but useful.  
showPlays :: [Play] -> String
showPlays ps = unlines $ map showPlayPretty ps

-----------------CODE FOR NAIVE ALGORITHM USING HASH TABLE----------------------
--returns all valid plays (words in dict) at some index and some max length
--getGoodPlaysAt :: BogHashTable -> BogGame -> (Int,Int) -> Int -> [Play]
--getGoodPlaysAt ht bg ix len = 
--    [ p | p <- getAllPlaysAt bg ix len , BoggleHash.inDict ht (snd p)]

--Take a boggle game, the current index, the remaining length, plays so far
--Returns all possible plays with (word size <= len)
--Recursively adds letters to each play provided there's an adjacent cube
--that hasn't been used yet.
getAllPlaysAtHash :: BogGame -> (Int, Int) -> BogHashTable -> Int -> [Play]
getAllPlaysAtHash (BogGame n board) ix hash len = 
    playsToSet [ (reverse $ fst p, reverse $ snd p) | p <- reversedPlays ]
        where reversedPlays = getAllPlaysAtHash' bg ix hash (len-1) nilPlay
              bg = BogGame n board

---------------------------OLD VERSION-----------------------------------------
--Strat is to add current letter to the move we're building, then do a 
--recursive call for each adjacent cube.
--getAllPlaysAtHash' :: BogGame -> (Int, Int) -> Int -> [Play] -> [Play]
--getAllPlaysAt' (BogGame size g) (n,m) len plays 
--    | len == 0 = []
--    | concatPlays == [] = [] --happens when we've visited all adj. cubes already
--    | otherwise = concatPlays ++ concat [ getAllPlaysAt' bg ix (len - 1) concatPlays | 
--            ix <- newCubes]
--        where diffs = [(-1,-1),(-1,0),(0,-1),(1,0),(0,1),(1,1),(-1,1),(1,-1)]
--              newCubes = [ (i+n,j+m) | (i,j) <- diffs, 
--                                 i+n >= 0, i+n < size, 
--                                 j+m >= 0, j+m < size ]
--              bg = (BogGame size g)
--              concatPlays = newPlays bg plays (n,m)

--getAllPlaysAtHash' :: BogGame -> (Int, Int) -> BogHashTable -> Int -> Play -> [Play]
--getAllPlaysAtHash' (BogGame size bg) (n,m) hash maxLen play 
--    | maxLen == 0 = addPlay
--    | otherwise = addPlay ++ concat [getAllPlaysAtHash' (BogGame size bg) 
--        (head $ fst play') 
--        hash
--        (maxLen - 1)
--        play' | play' <- newPlays]
--            where diffs = [(-1,-1),(-1,0),(0,-1),(1,0),(0,1),(1,1),(-1,1),(1,-1)]
--                  newPlays = [ ((i+n,j+m) : fst play, (bg ! (i+n,j+m)): snd play) 
--                        | (i,j) <- diffs, 
--                          isValidCubeHash (BogGame size bg) (i+n,j+m) play]
--                  addPlay = if BoggleHash.inDict hash (reverse $ snd play) then [play] else []

getAllPlaysAtHash' :: BogGame -> (Int, Int) -> BogHashTable -> Int -> Play -> [Play]
getAllPlaysAtHash' (BogGame size bg) (n,m) hash maxLen play 
    | maxLen == 0 = addPlay
    | otherwise = addPlay ++ concat [getAllPlaysAtHash' (BogGame size bg) 
        ix hash (maxLen - 1) newPlay | ix <- newCubes]
            where diffs = [(-1,-1),(-1,0),(0,-1),(1,0),(0,1),(1,1),(-1,1),(1,-1)]
                  newCubes = [ (i+n,j+m) | (i,j) <- diffs, 
                          isValidCubeHash (BogGame size bg) (i+n,j+m) newPlay]
                  newPlay = ((n,m) : fst play, (bg ! (n,m)) : snd play)
                  addPlay = if BoggleHash.inDict hash (reverse $ snd newPlay) then [newPlay] else []

--Helper to check if we should consider adding an adjacent cube.
isValidCubeHash :: BogGame -> (Int,Int) -> Play -> Bool
isValidCubeHash (BogGame size bg) (i,j) play 
    | i < 0 || j < 0 || i >= size || j >= size = False
    | elem (i,j) (fst play) = False
    | otherwise = True

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
solveGameHash :: BogGame -> BogHashTable -> Int -> [Play] 
solveGameHash (BogGame n board) ht maxLen =  
    filter ((>2) . length . snd) $ playsToSet 
        [ play | i <- [0..n-1], j <- [0..n-1], 
            play <- getAllPlaysAtHash (BogGame n board) (i,j) ht maxLen ]

--simple utility function 
listToSet :: (Eq a) => [a] -> [a] 
listToSet xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs

--Remove duplicate words from list of plays. It doesn't matter which play we
--choose if there are 2 ways to get a word. 
playsToSet :: [Play] -> [Play]
playsToSet xs = foldr (\x acc -> if snd x `elem` getWords acc then acc else x:acc) [] xs
    where getWords ws = map snd ws

------------------BOGGLE SOLVER USING THE TRIE STRUCTURE------------------------
--Get all plays rooted at a particular index. 
--The initial call to getAllPlaysAtTrie' uses the empty move ([],""). 
--Reverse the backwards words / moves returned by getAllPlaysAtTrie'
--Remove duplicate words with playsToSet
getAllPlaysAtTrie :: BogGame -> (Int, Int) -> BogTrie -> [Play]
getAllPlaysAtTrie game ix trie = [(reverse $ fst play, reverse $ snd play) | 
                    play <- getAllPlaysAtTrie' game ix trie ([], "")]

getAllPlaysAtTrie' :: BogGame -> (Int, Int) -> BogTrie -> Play -> [Play]
getAllPlaysAtTrie' (BogGame size bg) (n,m) trie play = 
    addPlay ++ concat [getAllPlaysAtTrie' (BogGame size bg) 
        (head $ fst play') 
        (getJustTrie $ lookup (head $ snd play') (branches trie)) 
        play' | play' <- newPlays]
    where diffs = [(0,0),(-1,-1),(-1,0),(0,-1),(1,0),(0,1),(1,1),(-1,1),(1,-1)]
          newPlays = [ ((i+n,j+m) : fst play, (bg ! (i+n,j+m)): snd play) 
                                | (i,j) <- diffs, 
                                    isValidCubeTrie (BogGame size bg) (i+n,j+m) trie play]
          addPlay = if realWord trie then [play] else []

--Helper to check if new cube is a valid move. 
--Uses trie to see if it's a substring of some actual word in dictionary.
isValidCubeTrie :: BogGame -> (Int,Int) -> BogTrie -> Play -> Bool
isValidCubeTrie (BogGame size bg) (n,m) trie pl 
    | n < 0 || m < 0 || n >= size || m >= size = False 
    | elem (n,m) (fst pl) = False
    | isNothing $ lookup (bg ! (n,m)) (branches trie)  = False
    | otherwise = True

getJustTrie :: Maybe BogTrie -> BogTrie --Get rid of Maybe with a default trie
getJustTrie Nothing = BogTrie False []
getJustTrie (Just t) = t

--Output all valid plays rooted at every possible index (i,j)
solveGameTrie :: BogGame -> BogTrie -> [Play]
solveGameTrie (BogGame size bg) trie = filter ((>2) . length . snd) $ playsToSet $ 
                    concat [getAllPlaysAtTrie (BogGame size bg) (i,j) trie | 
                        i <- [0..size-1], j <- [0..size-1]]

-------------------FUNCTIONS TO DISPLAY FINAL RESULTS---------------------------
--Helpers to compare plays based on length and lexicographic order of word
compLenP :: Play -> Play -> Ordering
compLenP p1 p2 | (length $ snd $ p1) < (length $ snd $ p2) = LT
               | (length $ snd $ p1) > (length $ snd $ p2) = GT
               | otherwise = EQ

compAlphaP :: Play -> Play -> Ordering
compAlphaP p1 p2 | (snd p1 < snd p2) = LT
                 | (snd p1 > snd p2) = GT
                 | otherwise = EQ

--Sort by length then lexicographic order
sortPlays :: [Play] -> [Play]
sortPlays = (sortBy compAlphaP) . (sortBy compLenP)

--group plays by length in increasing order. Throw out len 2 or less
groupByScore :: [Play] -> [[Play]]
groupByScore plays = [ [p | p <- plays, length (snd p) == aLen] | 
    aLen <- listToSet $ sort $ filter (>2) $ map (length . snd) plays]

--Get total score
totalScore :: [Play] -> Int
totalScore ps = sum $ map (score . snd) ps

--output String of 1 play per line broken down by score
showPlaysByScore :: [Play] -> String
showPlaysByScore ps = case groupByScore ps of 
                        []       -> "" 
                        (ps:pss) -> "\n" 
                            ++ (show $ score $ snd $ head ps) ++ " POINT WORDS "
                            ++ (if len == 3 then "(Length 3)\n" else 
                                if len == 4 then "(Length 4)\n" else 
                                    "\n")
                            ++ showPlays ps
                            ++ showPlaysByScore (concat pss)
                            where len = length $ snd $ head ps

--Append total score to the end
showPlaysByScoreWithTotal :: [Play] -> String
showPlaysByScoreWithTotal ps = showPlaysByScore ps 
                                ++ "\nTOTAL SCORE: " 
                                ++ show (totalScore ps)
    

--FINAL FUNCTIONS TO TAKE a Boggle Game + datastructure to String of solutions--
showSolnsTrie :: BogGame -> BogTrie -> String
showSolnsTrie game trie = 
    showPlaysByScoreWithTotal $ playsToSet $ solveGameTrie game trie

showSolnsHash :: BogGame -> BogHashTable -> Int -> String
showSolnsHash game ht maxLen = 
    showPlaysByScoreWithTotal $ playsToSet $ solveGameHash game ht maxLen
