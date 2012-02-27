module BoggleTrie where
import BoggleGame
import Treedot

--Citing Wikipedia here:http://en.wikipedia.org/wiki/Trie. 
--Saw sample code and used something similar for the data type. 
--However, the real work is in the algorithm to generate the trie
--and check if a string could be a prefix to a word.
data BogTrie = BogTrie { realWord :: Bool, branches :: [(Char,BogTrie)] } 
data LabelTrie = LabelTrie { lRealWord :: Bool, hash :: String, lBranches :: [(Char,LabelTrie)] } 
    deriving Show

instance Tree BogTrie where
    subtrees t = [snd p | p <- branches t]

instance Tree LabelTrie where
    subtrees t = [snd p | p <- lBranches t]

instance LabeledTree LabelTrie where 
    label t = hash t ++ "," ++ (if lRealWord t then "T" else "F")

--Again a bit similar to what I saw on Wikipedia, but different than a simple
--lookup. Also the real work is below (wanted practice using case statements) 
isPrefix :: String -> BogTrie -> Bool
isPrefix [] _ = True --If we get to a node and have no chars left, it's a prefix
isPrefix (x:xs) t = case subtree of 
                        Nothing -> False --If we have a char left but no subtree
                        Just t1 -> isPrefix xs t1
    where subtree = lookup x (branches t)

inDict :: String -> BogTrie -> Bool
inDict [] t = realWord t
inDict (x:xs) t = case subtree of 
                    Nothing -> False --Char left and no subtree, can't be a word
                    Just t1 -> inDict xs t1
    where subtree = lookup x (branches t)

--Takes the dictionary as a huge string, the current Trie, and gives the result
buildTrie :: [String] -> BogTrie
buildTrie [""] = BogTrie True [] --Base case: leaf is a word
buildTrie ws = BogTrie isWord [(fst l, buildTrie $ snd l) | l <- splits] 
    where splits = splitByLetter (if isWord then tail ws else ws)
          isWord = head ws == ""

--Takes the dictionary as a huge string, the current Trie, and gives the result
buildLabelTrie :: [String] -> String -> LabelTrie
buildLabelTrie [""] w = LabelTrie True w [] --Base case: leaf is a word
buildLabelTrie ws w = LabelTrie isWord w [(fst l, buildLabelTrie (snd l) (w ++ [fst l])) | l <- splits] 
    where splits = splitByLetter (if isWord then tail ws else ws)
          isWord = head ws == ""

splitByLetter :: [String] -> [(Char, [String])]
splitByLetter [] = []
splitByLetter xs = (c, getTruncs xs c) : splitByLetter (getRest xs c)
    where c = head (head xs)

getTruncs :: [String] -> Char -> [String]
getTruncs xs c = map tail $ takeWhile ((==c) . head) xs

getRest :: [String] -> Char -> [String]
getRest xs c = dropWhile ((==c) . head) xs


