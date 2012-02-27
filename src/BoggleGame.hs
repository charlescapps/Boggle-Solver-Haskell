module BoggleGame where
import Data.Array
import Data.Char

--size of board, then a 2d array with chars
data BogGame = BogGame Int (Array (Int,Int) Char)

--Define these defaults. Also have functions for arbitrary size.
bogSize::Int --number of rows/cols
bogSize = 4

maxIx::Int --Maximum index into boggle matrix, i.e. it's 4x4
maxIx = bogSize - 1

-------------------------display and parsing------------------------------------
instance Show BogGame where
    show (BogGame n g) = unlines $ map concat $ map (map (:" ")) 
			 [ [g ! (i,j) | j <- [0..n-1]] | i <- [0..n-1]]

--reduce the input board to a string of just letters (row major order)
--then insert letters into array based on index into list.
readGame :: String -> BogGame
readGame s = BogGame bogSize $
    array ((0,0),(maxIx,maxIx)) 
	[((i,j), letters !! (i*bogSize+j)) | i <- [0..maxIx],j <- [0..maxIx]]
		where letters = concat $ concat $ map words $ lines s

--read game from string given size n
readGameN :: String -> Int -> BogGame --construct arbitrary size game
readGameN s n = BogGame n $
    array ((0,0),(n-1,n-1)) 
	[((i,j), toUpper $ letters !! (i*n+j)) | i <- [0..n-1],j <- [0..n-1]]
		where letters = concat $ concat $ map words $ lines s

----------------------------SCORE CALCULATION-----------------------------------
score :: String -> Int
score word | len < 3 = 0
           | len == 3 || len == 4 = 1
           | len == 5 = 2
           | len == 6 = 3
           | len == 7 = 5
           | otherwise = 11
    where len = length word
