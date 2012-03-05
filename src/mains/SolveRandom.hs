module Main where
import BoggleSolver
import BoggleGame
import BoggleHash
import BoggleTest
import BoggleTrie
import BoggleRandom
import UtilityFuncs
import Treedot
import System.IO
import System

usageStr :: String
usageStr = "Need to specifiy 3 or 4 arguments.\n" 
            ++ "\tUsage: main \"dictfile\" (\"HASH\" | \"TRIE\") \"BOARDSIZE\" [MAX_WORD_LENGTH]\n"
            ++ "\tNote: Dictionary must have one uppercase word per line"


main::IO()
main = 
 do args <- getArgs                                                      
    if (length (args) /= 3 && length(args) /= 4)                                                
        then putStrLn usageStr 
    else    do let dictFileName = head args
               let dataStructure = args !! 1
               let boardSize = read (args !! 2)
               let maxLen = read (args !! 3)
               if dataStructure /= "HASH" && dataStructure /= "TRIE" then
                    putStrLn "Invalid datastructure. Must be HASH or TRIE."
               else return ()


               do putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                

               dictContents <- readFile dictFileName >>= (return . lines)                 
               let dictHashTable = if dataStructure == "HASH" then 
                        buildHashTable dictContents
                    else buildHashTable [] 
               let dictTrie = if dataStructure == "TRIE" then 
                        buildTrie dictContents
                    else buildTrie []

               --Print the max bucket size in the hash table
               putStrLn ("Max bucket size in hash table: " ++ 
                    (show . maxBucket $ dictHashTable) )

               --Get max word length in dictionary.
               let maxWordSize = (maximum . map length) dictContents
               putStrLn $ "Max word size in dict: " ++ (show maxWordSize) ++ "\n"

               if dataStructure == "HASH" then
                    solveRandomGamesHash dictHashTable boardSize 1 maxLen
               else 
                    solveRandomGamesTrie dictTrie boardSize 1


--Generate num random n by n games, then solve them and output results. lol. 
solveRandomGamesHash :: BogHashTable -> Int -> Int -> Int -> IO()
solveRandomGamesHash ht boardSize numSamples maxLen = 
    do randGames <- getGameSamples boardSize numSamples
       let showGames = map (putStrLn . show) randGames
       let showSolns = map putStrLn $ map (\game -> showSolnsHash game ht maxLen) randGames
       let showGameThenSoln = mixLists showGames showSolns
       executeList showGameThenSoln
       
--Generate num random n by n games, then solve them and output results. lol. 
solveRandomGamesTrie :: BogTrie -> Int -> Int -> IO()
solveRandomGamesTrie trie boardSize numSamples = 
    do randGames <- getGameSamples boardSize numSamples
       let showGames = map (putStrLn . show) randGames
       let showSolns = map putStrLn $ map (\game -> showSolnsTrie game trie) randGames
       let showGameThenSoln = mixLists showGames showSolns
       executeList showGameThenSoln



