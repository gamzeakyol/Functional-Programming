import Prelude hiding(filter)
import Data.List 
import Data.Map hiding(filter)

type Words = [Char]

type Sentence = [Words]

type CharacterCount = [(Char, Int)]

-- Q1
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
dictCharCounts :: [Words] -> [(Words, CharacterCount)]
dictCharCounts []         = []
dictCharCounts (w:words') = (w, (wordCharCounts w)):(dictCharCounts words')

--Q4
dictWordsByCharCounts :: [(Words, CharacterCount)] -> [(CharacterCount, [Words])]
dictWordsByCharCounts ws = assocs (fromListWith (++) (changeKeys ws))
	where
		changeKeys :: [(Words, CharacterCount)] -> [(CharacterCount, [Words])]
		changeKeys [] = []
		changeKeys ((w, c):ws) = (c, [w]):(changeKeys ws)
		
--Q5
wordAnagrams :: Words -> [(CharacterCount, [Words])] -> [Words]
wordAnagrams word ws@((c, w):ws')
	| ws == []                  = []
	|(wordCharCounts word) == c = w
	| otherwise 	            = wordAnagrams word ws'

--Q6 
--charCountsSubsets :: CharacterCount -> [[CharacterCount]]
	
--Q7
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
