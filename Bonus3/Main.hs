import Prelude hiding(filter)
import Data.List 
import Data.Map hiding(filter)

type Words = [Char]

type Sentence = [Words]

type CharacterCount = [(Char, Int)]

-- Q1
-- ex. input: wordCharCounts "tea"
-- ex. output: [('a',1),('e',1),('t',1)]
wordCharCounts :: Words -> CharacterCount
wordCharCounts word = wordCharCounts' (sort word)
	where
		wordCharCounts' :: Words -> CharacterCount
		wordCharCounts' [] = []
		wordCharCounts' (w:word') = (w, (length (filter (==w) word))):(wordCharCounts' (removeChar w word'))
			where
				removeChar :: Char -> Words -> Words		
				removeChar _ [] = []
				removeChar x (y:ys) 
					| x == y    = removeChar x ys
					| otherwise = y : removeChar x ys

-- Q2
-- ex. input: sentenceCharCounts ["i", "love", "linux"]
-- ex. output: [('e',1),('i',2),('l',2),('n',1),('o',1),('u',1),('v',1),('x',1)]
sentenceCharCounts :: Sentence -> CharacterCount
sentenceCharCounts [] = []
sentenceCharCounts (s:sentence) = assocs (fromListWith (+) (concat (sentenceCharCounts' (s:sentence))))
	where
		sentenceCharCounts' :: Sentence -> [CharacterCount]
		sentenceCharCounts' sentence@(s:sentence')
			| sentence == []  = []
			| sentence == [s] = [wordCharCounts s]
			| otherwise       = (wordCharCounts s):(sentenceCharCounts' sentence')

-- Q3
-- ex. input: dictCharCounts ["tea", "all", "ate"]
-- ex. output: [("tea",[('a',1),('e',1),('t',1)]),("all",[('a',1),('l',2)]),("ate",[('a',1),('e',1),('t',1)])]
dictCharCounts :: [Words] -> [(Words, CharacterCount)]
dictCharCounts []         = []
dictCharCounts (w:words') = (w, (wordCharCounts w)):(dictCharCounts words')

--Q4
--ex. input: dictWordsByCharCounts (dictCharCounts ["tea", "all", "ate"])
--ex. output: [([('a',1),('e',1),('t',1)],["ate","tea"]),([('a',1),('l',2)],["all"])]
dictWordsByCharCounts :: [(Words, CharacterCount)] -> [(CharacterCount, [Words])]
dictWordsByCharCounts ws = assocs (fromListWith (++) (changeKeys ws))
	where
		changeKeys :: [(Words, CharacterCount)] -> [(CharacterCount, [Words])]
		changeKeys [] = []
		changeKeys ((w, c):ws) = (c, [w]):(changeKeys ws)
		
--Q5
--ex. input:  wordAnagrams "eat" (dictWordsByCharCounts (dictCharCounts ["tea", "all", "ate"]))
--ex. output: ["ate","tea"]
wordAnagrams :: Words -> [(CharacterCount, [Words])] -> [Words]
wordAnagrams word ws@((c, w):ws')
	| ws == []                  = []
	|(wordCharCounts word) == c = w
	| otherwise 	            = wordAnagrams word ws'

--Q6 
--charCountsSubsets :: CharacterCount -> [[CharacterCount]]
	
--Q7
-- ex. input: subtractCounts (wordCharCounts "seat") (wordCharCounts "ate")
-- ex. output: [('a',0),('e',0),('s',1),('t',0)]
subtractCounts :: CharacterCount -> CharacterCount -> CharacterCount
subtractCounts s1'@((w1, c1):s1) s2'@((w2, c2):s2)
	| s2' == []                = s1'
	| (s2 == []) && (w1 == w2) = [(w1, c1-c2)] ++ s1
	| (s2 == []) && (w1 /= w2) = [(w1, c1)] ++ subtractCounts s1 [(w2, c2)]
	| w1 == w2                 = [(w1, c1-c2)] ++ subtractCounts s1 s2
	| otherwise                = [(w1, c1)] ++ subtractCounts s1 s2'

--Q9
--main :: IO ()
--main = do 
--	putStrLn "Enter sentence:"
--	anagrams <- sentenceAnagrams
--	putStrLn (show anagrams)
