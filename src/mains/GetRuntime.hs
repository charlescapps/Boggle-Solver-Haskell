module GetRuntime where
import BoggleSolver
import BoggleGame
import BoggleHash
import BoggleTrie
import BoggleRandom
import BoggleTest
import UtilityFuncs
import Data.Bits
import System.IO
import System.CPUTime
import System

data RunTime = RunTime {dataStructure::String, boardSize::Int, plays::[Play], 
    time::Integer, precision::Integer }

instance Show RunTime where
    show rt = (dataStructure rt) ++ " " ++ 
              show (boardSize rt) ++ " " ++ 
              show (time rt) ++ " " ++ 
              show (totalScore $ plays rt) ++ " " ++
              show (length $ plays rt) ++ " " ++
              show (precision rt)

header :: String
header = "DATASTRUCTURE N TIME SCORE WORDS_FOUND PRECISION"

headerAvgTrie :: String
headerAvgTrie = "DATASTRUCTURE N AVG_TIME STD_DEV NUM_BOARDS"

headerAvgHash :: String
headerAvgHash = "DATASTRUCTURE N AVG_TIME STD_DEV NUM_BOARDS MAXLEN"

usageStr :: String
usageStr = "Need to specifiy 6 or 7 arguments.\n" 
            ++ "\tUsage: get_runtime \"dictfile\" (\"HASH\" | \"TRIE\") \"MIN N\" \"MAX N\" \"NUM_REPEAT\" \"OUTPUT_FILE\" \"AVG_FILE\" [MAX_WORD_LENGTH]\n"
            ++ "\tNote: Dictionary must have one uppercase word per line"

main::IO()
main = 
 do args <- getArgs                                                      
    if (length (args) /= 7 && length(args) /= 8)                                                
        then putStrLn usageStr 
    else    do let dictFileName = head args
               let dataStructure = args !! 1
               let boardSizeMin = read (args !! 2)
               let boardSizeMax = read (args !! 3)
               let numRepeat = read (args !! 4)
               let outputFileName = (args !! 5)
               let avgFileName = (args !! 6)
               let maxLen = read (args !! 7)

               if dataStructure /= "HASH" && dataStructure /= "TRIE" then
                    do putStrLn "Invalid datastructure. Must be HASH or TRIE."
                       exitWith (ExitFailure 1)
               else return ()

               do putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                

               dictContents <- readFile dictFileName >>= (return . lines)                 

               let dictHashTable = if dataStructure == "HASH" then 
                        buildHashTable dictContents
                    else buildHashTable [] 
               let dictTrie = if dataStructure == "TRIE" then 
                        buildTrie dictContents
                    else buildTrie []

               runtimes <- if dataStructure == "HASH" then
                    getRuntimesHash dictHashTable boardSizeMin boardSizeMax numRepeat maxLen
               else getRuntimesTrie dictTrie boardSizeMin boardSizeMax numRepeat

               --putStrLn $ showPlaysByScoreWithTotal (concat $ map plays runtimes)

               outputH <- openFile outputFileName AppendMode 
               hPutStr outputH (header ++ "\n")
               hPutStr outputH (unlines (map show runtimes))
               fileOpen <- hIsOpen outputH
               if fileOpen then hClose outputH else return ()

               outputAvgH <- openFile avgFileName AppendMode 
               if dataStructure == "TRIE" then
                    do hPutStr outputAvgH (headerAvgTrie ++ "\n")
                       hPutStr outputAvgH (getAggregateAllTrie runtimes)
               else 
                    do hPutStr outputAvgH (headerAvgHash ++ "\n")
                       hPutStr outputAvgH (getAggregateAllHash maxLen runtimes)

               fileOpen <- hIsOpen outputH
               if fileOpen then hClose outputH else return ()

               fileOpen2 <- hIsOpen outputAvgH
               if fileOpen2 then hClose outputAvgH else return ()
--Want: "datastructure size num_words total_points time" on each line

getRuntimesHash :: BogHashTable -> Int -> Int -> Int -> Int -> IO [RunTime]
getRuntimesHash ht minSize maxSize numRepeat maxLen = 
    if minSize > maxSize then return [] else 
        do results <- getRuntimeOneSizeHash ht minSize numRepeat maxLen
           theRest <- getRuntimesHash ht (minSize + 1) maxSize numRepeat maxLen
           return (results ++ theRest)

getRuntimeOneSizeHash :: BogHashTable -> Int -> Int -> Int -> IO [RunTime]
getRuntimeOneSizeHash ht size numRepeat maxLen = 
    if numRepeat == 0 then return [] else
      do   randGame <- getGameSamples size 1
           startTime <- getCPUTime
           let plays = solveGameHash (head randGame) ht maxLen
           putStrLn $ show $ head randGame
           putStrLn $ showPlaysByScoreWithTotal plays
           endTime <- getCPUTime
           let precision = cpuTimePrecision
           let runTime = RunTime "HASH" size plays ((endTime-startTime) `div` 1000000) (precision `div` 1000000)
           theRest <- getRuntimeOneSizeHash ht size (numRepeat-1) maxLen
           return (runTime: theRest)

getRuntimesTrie :: BogTrie -> Int -> Int -> Int -> IO [RunTime]
getRuntimesTrie trie minSize maxSize numRepeat = 
    if minSize > maxSize then return [] else 
        do results <- getRuntimeOneSizeTrie trie minSize numRepeat
           theRest <- getRuntimesTrie trie (minSize + 1) maxSize numRepeat
           return (results ++ theRest)

getRuntimeOneSizeTrie :: BogTrie -> Int -> Int -> IO [RunTime]
getRuntimeOneSizeTrie trie size numRepeat = 
    if numRepeat == 0 then return [] else
      do   randGame <- getGameSamples size 1
           startTime <- getCPUTime
           let plays = solveGameTrie (head randGame) trie 
           putStrLn $ show $ head randGame
           putStrLn $ showPlaysByScoreWithTotal plays
           endTime <- getCPUTime
           let precision = cpuTimePrecision
           let runTime = RunTime "TRIE" size plays ((endTime-startTime) `div` 1000000) (precision `div` 1000000)
           theRest <- getRuntimeOneSizeTrie trie size (numRepeat-1)
           return (runTime: theRest)

groupBySize :: [RunTime] -> [[RunTime]]
groupBySize [] = []
groupBySize rts = takeWhile (\x -> (boardSize x == size)) rts : 
    groupBySize (dropWhile (\x  -> (boardSize x == size)) rts)
    where size = boardSize (head rts)

avgRuntime ::  [RunTime] -> Double
avgRuntime rts = (sum times) / fromIntegral (length times)
    where times = getTimes rts

getTimes :: [RunTime] -> [Double]
getTimes rts = map (fromIntegral . time) rts

stdDev :: [Double] -> Double
stdDev ds = sqrt ((sum squares) / fromIntegral (length ds))
    where avg = sum ds / fromIntegral (length ds)
          squares = map (\x -> (x-avg)*(x-avg) ) ds

getAvg :: [Double] -> Double
getAvg ds = sum ds / fromIntegral (length ds) 

getAggregateStrTrie :: [RunTime] -> String
getAggregateStrTrie rs = "TRIE " ++ (show n) ++ " " ++ (show $ getAvg times) ++ " " ++ (show $ stdDev times) 
                            ++ " " ++ (show $ length rs)
    where times = getTimes rs
          n = boardSize (head rs)

getAggregateAllTrie :: [RunTime] -> String
getAggregateAllTrie rs = unlines $ map getAggregateStrTrie groups
    where groups = groupBySize rs

getAggregateStrHash :: Int -> [RunTime] -> String
getAggregateStrHash maxLen rs = "HASH " ++ (show n) ++ " " ++ (show $ getAvg times) ++ " " ++ (show $ stdDev times) 
                            ++ " " ++ (show $ length rs) ++ " " ++ (show maxLen)
    where times = getTimes rs
          n = boardSize (head rs)

getAggregateAllHash :: Int -> [RunTime] -> String
getAggregateAllHash maxLen rs = unlines $ map (getAggregateStrHash maxLen) groups
    where groups = groupBySize rs
