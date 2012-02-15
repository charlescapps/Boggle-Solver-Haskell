module Main where
import BogHash
import System.IO
import System

readInt :: String -> Int
readInt s = read s

main::IO()
main = do args <- getArgs                                                      
          if (length (args) /= 2)                                                
              then putStrLn ("Need to specifiy a dictionary and size of hash table. \nUsage: Main \"dictfile\" <hash_size> \n"
                            ++ "Dictionary must be a line-separated list of uppercase words")
          else do let dictFileName = head args                                     
                  let hashSize = read $ head (tail args)
                  putStrLn ("Hash table size: " ++ show hashSize)

                  putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                
                  dictContents <- readFile dictFileName                  
                  myHashTable <- return $ getBogHashTableN (lines dictContents) hashSize

                  putStrLn ("Max bucket size: " ++ 
                        (show . maxBucket $ myHashTable) ++ "\n")



                  





                  --putStrLn $ "Some buckets:\n" ++ (unlines $ map show $ getBuckets myHashTable [1,128,256,512])


