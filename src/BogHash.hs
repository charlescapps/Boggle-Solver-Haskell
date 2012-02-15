module BogHash where
import Data.Array
import Data.Ix
import Data.Bits
import Char (digitToInt)

--Hash table with [String] for each bucket, uses a hash function String -> Int
type BogHashTable = Array Int [String]

--Number of buckets in table. Will experi
hashMax::Int
hashMax = shift 1 20 -- ~1,000,000 buckets, ~28,000 used, max bucket = 144
--When I decrease the no. to 2^19 the max bucket size increases to 254!!
--so 2^20 seems like a good balance. Should be around 4 MB + overhead

hashRange::(Int,Int)
hashRange = (0, hashMax-1)

--compute hash in range [0...2^20-1]. Whether a letter is present or not
wordHash::String->Int
wordHash s = wordHash' s `mod` hashMax

--compute hash in range [0...n-1]. Whether a letter is present or not
wordHashN::String -> Int -> Int
wordHashN s n = wordHash' s `mod` n

--Helper that computes the hash *before* taking the mod.
wordHash' :: String -> Int 
wordHash' [] = 0
wordHash' (c:cs) = charToBit c .|. wordHash' cs

--'a'-> 1, 'b'-> 2, 'c' -> 4, 'd' -> 8 ...
charToBit::Char -> Int
charToBit c = shift 1 (fromEnum c - fromEnum 'A')

getBogHashTable::[String] -> BogHashTable  
getBogHashTable dict = accumArray (\ws w -> w:ws) [] hashRange 
                [(wordHash word, word) | word <- dict]  

--flexible version where we can pass in max hash value
getBogHashTableN::[String] -> Int -> BogHashTable  
getBogHashTableN dict n = accumArray (\ws w -> w:ws) [] (0,n-1) 
                [(wordHashN word n, word) | word <- dict]  

--Get Bucket given String to hash
queryHash::BogHashTable -> String -> [String]
queryHash ht word = ht ! (wordHashN word (snd (bounds ht) + 1 ))

--In dictionary?
inDict::BogHashTable -> String -> Bool
inDict ht word = elem word (ht ! (wordHash word))

--Display function
showBogHash::BogHashTable -> String
showBogHash ht = concat $ [ "[" ++ show i ++ ", \n" ++ 
    concat (map (++"\n") (ht!i)) ++ "]\n" 
    | i <- [0..hashMax-1], (ht!i) /= []]

--Compute the no. of elements in the fullest bucket
maxBucket::BogHashTable -> Int
maxBucket ht = foldl (\nom nomNom -> max (length (ht ! nomNom)) nom ) 0 [0..snd (bounds ht)]

--Misc. Test functions. This gets a list of buckets given some hash values
getBuckets::BogHashTable -> [Int] -> [[String]]
getBuckets ht xs = [(ht!i) | i <- xs]
