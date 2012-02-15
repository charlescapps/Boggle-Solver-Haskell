module Main where
import GenPlays
import BoggleData
import BogHash
import System.IO
import System

readInt :: String -> Int
readInt s = read s

main::IO()
main = do args <- getArgs                                                      
          if (length (args) /= 2)                                                
              then putStrLn ("Need to specifiy a dictionary and boggle game file. \nUsage: main \"dictfile\" \"gamefile\"\n"
                            ++ "Note: Dictionary must be line-separated list of uppercase words")
          else do let dictFileName = head args                                     
                  let gameFileName = head (tail args)

                  putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                
                  dictContents <- readFile dictFileName                  
                  myHashTable <- return $ getBogHashTable $ lines dictContents

                  putStrLn ("Max bucket size: " ++ 
                        (show . maxBucket $ myHashTable) ++ "\n")

                  --read in board file and create a board object
                  gameContents <- readFile gameFileName
                  let gameLines = lines gameContents
                  let boardSize = readInt $ head gameLines
                  let myBoard = readGameN (concat $ tail gameLines) boardSize 

                  putStrLn $ show myBoard
                  putStrLn "All plays at (0,0) of len <= 3 :" 
                  let plays = getAllPlaysAt myBoard (0,0) 3
                  putStrLn $ showPlays plays

                  putStrLn $ show myBoard
                  putStrLn "All *good* plays at (2,2) of len <= 7 :" 
                  let plays = getGoodPlaysAt myHashTable myBoard (2,2) 7
                  putStrLn $ showPlays plays

                  





                  --putStrLn $ "Some buckets:\n" ++ (unlines $ map show $ getBuckets myHashTable [1,128,256,512])


