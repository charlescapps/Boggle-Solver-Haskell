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
import Test.QuickCheck
import Test.QuickCheck.Gen

usageStr :: String
usageStr = "Need to specifiy a dictionary file.\n" 
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

--Do QuickChecks on a random sample of boards of sizes 4x4...20x20
--Random start positions (ixes)
--Output valid moves only
--testRandomGoodPlays :: BogHashTable -> Int -> IO ()
--testRandomGoodPlays ht n = 
--    do randGamesIxes <- sample' (arbitrary::Gen GameIndex)
--       let games = map getGame randGamesIxes
--       let ixes = map getIxes randGamesIxes
--       let results = zipWith (\g ix -> getGoodPlaysAt ht g ix n) games ixes
--       let showGames = map (putStrLn . show) games
--       let showResults = map (putStrLn . showPlays) results
--       let showAll = mixLists showGames showResults
--       executeList showAll
--
----Output valid moves and bogus moves from random quickChecks. 
--testRandomAll :: BogHashTable -> Int -> IO ()
--testRandomAll ht n = 
--    do randGamesIxes <- sample' (arbitrary::Gen GameIndex)
--       let games = map getGame randGamesIxes
--       let ixes = map getIxes randGamesIxes
--       let goodPlays = zipWith (\g ix -> getGoodPlaysAt ht g ix n) games ixes
--       let allPlays = zipWith (\g ix -> getAllPlaysAt g ix n) games ixes
--       let showGames = map (putStrLn . show) games
--       let showGoodPlays = map (putStrLn . ("GOOD PLAYS:\n" ++ ) . showPlays) goodPlays
--       let showAllPlays = map (putStrLn . ("ALL PLAYS:\n" ++ ) . showPlays) allPlays
--       let showAll = mixLists3 showGames showGoodPlays showAllPlays
--       executeList showAll

--build an example trie. Shouldn't use the whole dictionary when creating a 
--labeled trie. Input is dictionary split into lines and filename
buildSmallTrie::[String] -> String -> IO() 
buildSmallTrie dict fileName = 
    do let t = buildLabelTrie dict ""
       putStrLn $ "Writing dot file " ++ fileName
       writeFile fileName (toDot t)


