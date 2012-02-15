module GenPlays where
import Data.Array
import BoggleData
import BogHash

--represents a snakey path through the boggle matrix
type Play = ([(Int,Int)],String) --list of indices into matrix and the word

--getValidPlays :: BogHashTable -> BoggleGame -> [Play]


getGoodPlaysAt :: BogHashTable -> BoggleGame -> (Int,Int) -> Int -> [Play]
getGoodPlaysAt ht bg ix len = 
    [ p | p <- getAllPlaysAt bg ix len [], inDict ht (snd p)]

--Take a boggle game, the current index, the remaining length, plays so far
--Returns all possible plays with (word size <= len)
--Recursively adds letters to each play provided there's an adjacent cube
--that hasn't been used yet.
getAllPlaysAt :: BoggleGame -> (Int, Int) -> Int -> [Play] -> [Play]
getAllPlaysAt bg ix len plays = 
    [ (reverse $ fst p, reverse $ snd p) | p <- reversedPlays ]
        where reversedPlays = getAllPlaysAt' bg ix len plays

getAllPlaysAt' :: BoggleGame -> (Int, Int) -> Int -> [Play] -> [Play]
getAllPlaysAt' (BoggleGame size g) (n,m) len plays 
    | len == 0 = []
    | concatPlays == [] = []
    | otherwise = concatPlays ++ concat [ getAllPlaysAt' bg ix (len - 1) concatPlays | 
            ix <- newCubes]
        where diffs = [(-1,-1), (-1,0), (0,-1), (1,0), (0,1), (1,1), (-1,1), (1,-1)]
              newCubes = [ (i+n,j+m) | (i,j) <- diffs, 
                                 i+n >= 0, i+n < size, 
                                 j+m >= 0, j+m < size ]
              bg = (BoggleGame size g)
              concatPlays = newPlays bg plays (n,m)


newPlays :: BoggleGame -> [Play] -> (Int, Int) -> [Play]
newPlays (BoggleGame n g) plays ix
    | plays == [] = [([ix], [g!ix])]
    | otherwise = [ ( ix:(fst p), (g!ix) : (snd p) ) | 
            p <- plays, not $ ix `elem` (fst p) ]
            

showPlays :: [Play] -> String
showPlays ps = unlines $ map show ps

filterPlays :: (Int, Int) -> [Play] -> [Play]
filterPlays ix plays = [ p | p <- plays, not $ ix `elem` (fst p)]
