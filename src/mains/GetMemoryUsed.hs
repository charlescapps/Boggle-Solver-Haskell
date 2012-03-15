module GetMemoryUsed where
import BoggleSolver
import BoggleGame
import BoggleHash
import BoggleTrie
import BoggleRandom
import BoggleTest
import UtilityFuncs
import Data.Bits
import Data.Array
import System.IO
import System.CPUTime
import System


usageStr :: String
usageStr = "get_memory_used \"dict_file\" \"(TRIE | HASH | NONE)\""

main::IO()
main = 
 do args <- getArgs                                                      
    if (length (args) /= 2 )
        then putStrLn usageStr 
    else    do let dictFileName = head args
               let dataStructure = args !! 1

               if dataStructure /= "HASH" && dataStructure /= "TRIE" && dataStructure /= "NONE" then
                    do putStrLn "Invalid datastructure. Must be HASH or TRIE."
                       exitWith (ExitFailure 1)
               else return ()

               do putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                

               dictContents <- if dataStructure == "NONE" then return [] else 
                    readFile dictFileName >>= (return . lines)                 

               dictTrie <- return $! if dataStructure == "TRIE" then 
                        buildTrie dictContents
                    else BogTrie False []

               dictHashTable <- return $! if dataStructure == "HASH" then 
                        buildHashTable dictContents
                    else array (0,0) [(0,[])]

               putStrLn "Done building structures"

               putStrLn $! (show dictTrie)
               putStrLn $! (show dictHashTable)

               putStrLn $! "Done showing structures"

               c <- getChar

               return ()

