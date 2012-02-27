module Main where
import BoggleSolver
import BoggleGame
import BoggleHash
import BoggleTest
import BoggleTrie
import BoggleRandom
import Treedot
import System.IO
import System
import Test.QuickCheck
import Test.QuickCheck.Gen

main::IO()
main = 
    do args <- getArgs                                                      
       if (length (args) /= 2)                                                
            then putStrLn ("Need to specifiy a dictionary and boggle game file. \nUsage: main \"dictfile\" \"gamefile\"\n"
                        ++ "Note: Dictionary must be line-separated list of uppercase words")
       else do let dictFileName = head args                                     
               let gameFileName = head (tail args)

               --Load dictionary from file, build hash table
               putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                
               dictContents <- readFile dictFileName >>= (return . lines)                 
               myHashTable <- return $ getBogHashTable $ dictContents

               --Print the max bucket size in the hash table
               putStrLn ("Max bucket size: " ++ 
                    (show . maxBucket $ myHashTable) )

               --Get max word length in dictionary.
               maxWordSize <- return $ getMaxWordSize dictContents
               putStrLn $ "Max word size: " ++ (show maxWordSize) ++ "\n"

              --read in board file and create a board object
               gameContents <- readFile gameFileName >>= (return . lines)
               boardSize <- return $ read $ head gameContents
               myBoard <- return $ readGameN (concat $ tail gameContents) boardSize

               let solns = solvePretty myBoard myHashTable 10
               putStrLn solns

               --let dictTrie = buildTrie dictContents
               --let plays = getGoodPlaysAt myHashTable myBoard (1,3) 7
               --putStrLn (show plays)
               --testRandomGoodPlays myHashTable 3

               return ()
               --putStrLn ("Building dot of a small trie")
               --buildSmallTrie (lines dictContents "dots/trie.dot")

getMaxWordSize :: [String] -> Int
getMaxWordSize words = maximum $ map length words

       

--Do QuickChecks on a random sample of boards of sizes 4x4...20x20
--Random start positions (ixes)
--Output valid moves only
testRandomGoodPlays :: BogHashTable -> Int -> IO ()
testRandomGoodPlays ht n = 
    do randGamesIxes <- sample' (arbitrary::Gen GameIndex)
       let games = map getGame randGamesIxes
       let ixes = map getIxes randGamesIxes
       let results = zipWith (\g ix -> getGoodPlaysAt ht g ix n) games ixes
       let showGames = map (putStrLn . show) games
       let showResults = map (putStrLn . showPlays) results
       let showAll = intersperse showGames showResults
       executeList showAll

--Output valid moves and bogus moves from random quickChecks. 
testRandomAll :: BogHashTable -> Int -> IO ()
testRandomAll ht n = 
    do randGamesIxes <- sample' (arbitrary::Gen GameIndex)
       let games = map getGame randGamesIxes
       let ixes = map getIxes randGamesIxes
       let goodPlays = zipWith (\g ix -> getGoodPlaysAt ht g ix n) games ixes
       let allPlays = zipWith (\g ix -> getAllPlaysAt g ix n) games ixes
       let showGames = map (putStrLn . show) games
       let showGoodPlays = map (putStrLn . ("GOOD PLAYS:\n" ++ ) . showPlays) goodPlays
       let showAllPlays = map (putStrLn . ("ALL PLAYS:\n" ++ ) . showPlays) allPlays
       let showAll = intersperse3 showGames showGoodPlays showAllPlays
       executeList showAll

--build an example trie. Shouldn't use the whole dictionary when creating a 
--labeled trie. Input is dictionary split into lines and filename
buildSmallTrie::[String] -> String -> IO() 
buildSmallTrie dict fileName = 
    do let t = buildLabelTrie dict ""
       putStrLn $ "Writing dot file " ++ fileName
       writeFile fileName (toDot t)

--Helper to mix 2 lists with alternating entries
intersperse :: [a] -> [a] -> [a]
intersperse _ [] = []
intersperse [] _ = []
intersperse (x:xs) (y:ys) = x:y:(intersperse xs ys)

--Helper to mix 3 lists with alternating entries
intersperse3 :: [a] -> [a] -> [a] -> [a]
intersperse3 _ _ [] = []
intersperse3 _ [] _ = []
intersperse3 [] _ _ = []
intersperse3 (x:xs) (y:ys) (z:zs) = x:y:z:(intersperse3 xs ys zs)

--Helper to execute everything in a list. Like sequence but returns IO () type
executeList :: [IO a] -> IO ()
executeList xs = sequence xs >> return ()

