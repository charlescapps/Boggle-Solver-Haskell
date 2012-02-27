module BoggleRandom where
import Random
import System.IO
import BoggleGame

randUpper :: IO Char
randUpper = do index <- randomRIO (0, 25)
               return $ toEnum $ (fromEnum 'A') + index

randUpperStr :: Int -> IO String
randUpperStr n = sequence $ replicate n randUpper

--Takes N, NUM, SEED and gives a list of NUM NxN boggle games.
--getRandomGames :: Int -> Int -> Int -> [BogGame]

