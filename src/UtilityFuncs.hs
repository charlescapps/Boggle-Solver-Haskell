module UtilityFuncs where
import BoggleGame

----------------------------STRING HELPER FUNCTIONS----------------------------
--Helper to mix 2 lists with alternating entries
mixLists :: [a] -> [a] -> [a]
mixLists _ [] = []
mixLists [] _ = []
mixLists (x:xs) (y:ys) = x:y:(mixLists xs ys)

--Helper to mix 3 lists with alternating entries
mixLists3 :: [a] -> [a] -> [a] -> [a]
mixLists3 _ _ [] = []
mixLists3 _ [] _ = []
mixLists3 [] _ _ = []
mixLists3 (x:xs) (y:ys) (z:zs) = x:y:z:(mixLists3 xs ys zs)

----------------------------IO HELPER FUNCTIONS--------------------------------
--Helper to execute everything in a list. Like sequence but returns IO () type
executeList :: [IO a] -> IO ()
executeList xs = sequence xs >> return ()

---------------------------PARSING HELPER FUNCTIONS----------------------------
--read in board file format and create a board object
readGameFromFile :: String -> BogGame
readGameFromFile str = readGameN (concat $ tail fileLines) gameSize
    where fileLines = lines str
          gameSize = read $ head fileLines
