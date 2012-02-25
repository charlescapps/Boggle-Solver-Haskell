module Main where
import GenPlays
import BoggleData
import BogHash
import BoggleTest
import System.IO
import System
import Test.QuickCheck
import Test.QuickCheck.Gen

readInt :: String -> Int
readInt s = read s

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
               dictContents <- readFile dictFileName                  
               myHashTable <- return $ getBogHashTable $ lines dictContents

               --Print the max bucket size in the hash table
               putStrLn ("Max bucket size: " ++ 
                    (show . maxBucket $ myHashTable) ++ "\n")

              --read in board file and create a board object
               gameContents <- readFile gameFileName
               let gameLines = lines gameContents
               let boardSize = readInt $ head gameLines
               let myBoard = readGameN (concat $ tail gameLines) boardSize 

               testRandomBoards myHashTable 8

--Generate a lot of random boards and start indexes, then get solutions under
--some length 'n' starting at that index
testRandomBoards::BogHashTable -> Int -> IO ()
testRandomBoards ht n = 
    do randGamesIxes <- sample' (arbitrary::Gen GameIndex)
       let games = map getGame randGamesIxes
       let ixes = map getIxes randGamesIxes
       let results = zipWith (\g ix -> getGoodPlaysAt ht g ix n) games ixes
       let showGames = map (putStrLn . show) games
       let showResults = map (putStrLn . showPlays) results
       let showAll = intersperse showGames showResults
       executeList showAll


intersperse :: [a] -> [a] -> [a]
intersperse [] [] = []
intersperse (x:xs) (y:ys) = x:y:(intersperse xs ys)

executeList :: [IO a] -> IO ()
executeList [] = return ()
executeList (x:xs) = x >> (executeList xs)

