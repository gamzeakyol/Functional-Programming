import Prelude hiding(filter)
import Data.List 
import Data.Map hiding(filter)
import Data.Function (on)

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
--sentenceCharCounts :: Sentence -> CharacterCount
--sentenceCharCounts [] = []
--sentenceCharCounts (s:sentence) = assocs (fromListWith (++) concat (sentenceCharCounts' (s:sentence)))
--    where
--        sentenceCharCounts' :: Sentence -> [CharacterCount]
--        sentenceCharCounts' (s:sentence) = (wordCharCounts s):(sentenceCharCounts' sentence)

-- Q3
dictCharCounts :: [Words] -> [(Words, CharacterCount)]
dictCharCounts []         = []
dictCharCounts (w:words') = (w, (wordCharCounts w)):(dictCharCounts words')

-- Q4
dictWordsByCharCounts :: [(Words, CharacterCount)] -> [[(Words, CharacterCount)]]
dictWordsByCharCounts []                  = []
dictWordsByCharCounts ws                  = groupBy ((==) `on` snd) (sort ws)




