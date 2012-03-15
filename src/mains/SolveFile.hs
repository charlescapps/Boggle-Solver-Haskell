module SolveFile where
import BoggleSolver
import BoggleGame
import BoggleHash
import BoggleTest
import BoggleTrie
import BoggleRandom
import UtilityFuncs
import Treedot
import System.IO
import System.Exit
import System

usageStr :: String
usageStr = "Need to specifiy 3 or 4 arguments.\n" 
            ++ "\tUsage: main \"dictfile\" (\"HASH\" | \"TRIE\") \"BOARD_FILE\" [MAX_WORD_LENGTH]\n"
            ++ "\tNote: Dictionary must have one uppercase word per line"


main::IO()
main = 
 do args <- getArgs                                                      
    if (length (args) /= 3 && length(args) /= 4)      
        then putStrLn usageStr 
    else    do let dictFileName = head args
               let dataStructure = args !! 1
               let boardFile = args !! 2
               let maxLen = read (args !! 3)
               if dataStructure /= "HASH" && dataStructure /= "TRIE" then
                    do putStrLn "Invalid datastructure. Must be HASH or TRIE."
                       exitWith (ExitFailure 1)
               else return ()

               putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                

               dictContents <- readFile dictFileName >>= (return . lines)                 
               let dictHashTable = if dataStructure == "HASH" then 
                        buildHashTable dictContents
                    else buildHashTable [] 
               let dictTrie = if dataStructure == "TRIE" then 
                        buildTrie dictContents
                    else buildTrie []

               boardData <- readFile boardFile >>= return . lines
               let boardSize = read (head boardData)
               let boardFromFile = readGameN (unlines $ tail boardData) boardSize

               let solns = if dataStructure == "HASH" then
                         solveGameHash boardFromFile dictHashTable maxLen 
                    else solveGameTrie boardFromFile dictTrie

               putStrLn $ show boardFromFile
               putStrLn $ showPlaysByScoreWithTotal solns

