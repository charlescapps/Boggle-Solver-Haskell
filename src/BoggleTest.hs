module BoggleTest where
import BoggleGame
import BoggleHash
import BoggleSolver
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Array

--Generate arbitrary capital letters
getUpper::Gen Char
getUpper= oneof (map return ['A'..'Z'])

--Get arbitrary NxN string of uppercase letters
arbitString::Int -> Gen [Char] 
arbitString n | (n == 0) = return []
              | otherwise = do c <- getUpper
                               str <- (arbitString (n-1))
                               return (c:str)

--Generate arbitrary BogGame, 4x4 up to 20x20
instance Arbitrary BogGame where
    arbitrary = do n <- choose (4,20)
                   list <- arbitString (n*n)
                   return $ BogGame n (array ((0,0),(n-1,n-1))
                            [((i`div`n,i`mod`n), list !! i) | i <- [0..n*n-1] ])

--Get arbitrary nxn BogGame
getArbitraryGame :: Int -> Gen BogGame
getArbitraryGame n = do list <- arbitString (n*n)
                        return $ BogGame n (array ((0,0),(n-1,n-1))
                            [((i`div`n,i`mod`n), list !! i) | i <- [0..n*n-1] ])
                     
--Get num many arbitrary nxn BogGame's
getArbitraryGames :: Int -> Int -> Gen [BogGame]
getArbitraryGames n num = sequence $ replicate num (getArbitraryGame n)

--Make a type that's just a game and a starting index
--I can't get around needing this to get an arbitrary
--game & arbitrary index in the proper range
data GameIndex = GameIx BogGame (Int,Int)
    deriving Show

getGame::GameIndex -> BogGame
getGame (GameIx bg ixes) = bg

getIxes::GameIndex -> (Int,Int)
getIxes (GameIx bg ixes) = ixes

instance Arbitrary GameIndex where
    arbitrary = do (BogGame n g) <- arbitrary
                   ix1 <- choose (0,n-1)
                   ix2 <- choose (0,n-1)
                   return $ GameIx (BogGame n g) (ix1,ix2)

----------------------------TESTS-----------------------------

--If w1 contains some char, and w2 doesn't contain it, then hashes are different
--(At least if we have 2^26 buckets they are different!)
prop_hashReq :: Char -> String -> String -> Property
prop_hashReq c w1 w2= 
        (elem c w1) && not (elem c w2) ==>
            wordHashN w1 hashMaxAlphabet /= wordHashN w2 hashMaxAlphabet

--Check if the lists of coordinates are the same length as the word found
prop_lenOfPlays :: BogGame -> Bool
prop_lenOfPlays game
    = foldr (\x acc -> acc && (length $ fst x) == (length $ snd x)) True allPlays
        where allPlays = getAllPlaysAt game (0,0) 8


--Check if the lists of coordinates are the same length as the word found
prop_lenOfPlays' :: GameIndex -> Bool
prop_lenOfPlays' (GameIx game (i,j))
    = foldr (\x acc -> acc && (length $ fst x) == (length $ snd x)) True allPlays
        where allPlays = getAllPlaysAt game (i,j) 6
-----------------------COMMANDS-------------------------------
--Check 20 boggle games since it takes a long time to check large boards
test20 = quickCheckWith (Args Nothing 20 20 100 True) prop_lenOfPlays
test20' = quickCheckWith (Args Nothing 20 20 100 True) prop_lenOfPlays'




















-------------Test objects-----------------
test::String
test = "a a a a\na a a a\na a a a\na a a a"

b::BogGame
b = BogGame bogSize $ 
    array ((0,0),(bogSize,bogSize)) [((i,j),'a') | i <- [0..maxIx], j <- [0..maxIx]]
