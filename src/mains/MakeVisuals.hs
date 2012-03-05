module Main where
import TreeDot
import BoggleTrie

--build an example trie. Shouldn't use the whole dictionary when creating a 
--labeled trie. Input is dictionary split into lines and filename
dictToDotFile ::[String] -> String -> IO() 
dictToDotFile dict fileName = 
    do let t = buildLabelTrie dict ""
       putStrLn $ "Writing dot file " ++ fileName
       writeFile fileName (toDot t)

usageStr :: String
usageStr = "Need to specifiy a dictionary file and number of words.\n" 
            ++ "\tUsage: main \"dictfile\" \"num_words\" \n"
            ++ "\tNote: Dictionary must have one uppercase word per line"

main :: IO()
main = 
 do args <- getArgs                                                      
    if (length (args) /= 2)                                                
        then putStrLn usageStr 
    else do let dictFileName = head args
            let numWords = read (args !! 1)

            do putStrLn ("Dictionary Input from: '" ++ dictFileName ++ "'")                
            dictContents <- readFile dictFileName >>= (return . lines)                 
            return ()
