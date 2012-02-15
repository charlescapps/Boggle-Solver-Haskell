module BoggleData where
import Data.Array
import Data.Char

--size of board, then a 2d array with chars
data BoggleGame = BoggleGame Int (Array (Int,Int) Char)

--Define these so we could easily do 5x5 boggle, etc.

bogSize::Int --number of rows/cols
bogSize = 4

maxIx::Int --Maximum index into boggle matrix, i.e. it's 4x4
maxIx = bogSize - 1

-----------------display and parsing-------------------
instance Show BoggleGame where
    show (BoggleGame n g) = unlines $ map concat $ map (map (:" ")) 
			 [ [g ! (i,j) | j <- [0..n-1]] | i <- [0..n-1]]

--reduce the input board to a string of just letters (row major order)
--then insert letters into array based on index into list.
readGame :: String -> BoggleGame
readGame s = BoggleGame bogSize $
    array ((0,0),(maxIx,maxIx)) 
	[((i,j), letters !! (i*bogSize+j)) | i <- [0..maxIx],j <- [0..maxIx]]
		where letters = concat $ concat $ map words $ lines s

--read game from string given size n
readGameN :: String -> Int -> BoggleGame --construct arbitrary size game
readGameN s n = BoggleGame n $
    array ((0,0),(n-1,n-1)) 
	[((i,j), toUpper $ letters !! (i*n+j)) | i <- [0..n-1],j <- [0..n-1]]
		where letters = concat $ concat $ map words $ lines s

-----------------Generate all possible plays------------------







-------------Test objects-----------------
test::String
test = "a a a a\na a a a\na a a a\na a a a"

b::BoggleGame
b = BoggleGame bogSize $ 
    array ((0,0),(bogSize,bogSize)) [((i,j),'a') | i <- [0..maxIx], j <- [0..maxIx]]
