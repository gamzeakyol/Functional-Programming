import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving (Show, Eq)
type Word = String

--input: empty
--output: Trie {end = False, children = fromList []}
empty :: Trie
empty = Trie {end = False, children = M.empty}


--input: insert "ani" (insert "abla" empty)
--output: Trie {end = False, children = fromList [('a',Trie {end = False, children = fromList [('b',Trie {end = False, children = fromList [('l',Trie {end = False, children = fromList [('a',Trie {end = True, children = fromList []})]})]}),('n',Trie {end = False, children = fromList [('i',Trie {end = True, children = fromList []})]})]})]}

--input: insert "aniden" (insert "ani" empty)
--output: Trie {end = False, children = fromList [('a',Trie {end = False, children = fromList [('n',Trie {end = False, children = fromList [('i',Trie {end = True, children = fromList [('d',Trie {end = False, children = fromList [('e',Trie {end = False, children = fromList [('n',Trie {end = True, children = fromList []})]})]})]})]})]})]}		
insert :: Word -> Trie -> Trie
insert [] (Trie _ c)     = Trie True c
insert (w:ws) (Trie b c) = Trie b newChildren
	where
		newChildren = M.insert w (insert ws child) c
			where 
				child = fromMaybe empty (M.lookup w c)


--input: insertList ["abla", "an", "ana"]
--output: Trie {end = False, children = fromList [('a',Trie {end = False, children = fromList [('b',Trie {end = False, children = fromList [('l',Trie {end = False, children = fromList [('a',Trie {end = True, children = fromList []})]})]}),('n',Trie {end = True, children = fromList [('a',Trie {end = True, children = fromList []})]})]})]}	
insertList :: [Word] -> Trie
insertList [] = insert [] empty
insertList (w:ws) = foldr insert empty (w:ws)


--input: search "ana" (insert "ani" (insert "abla" empty))
--output: False

--input: search "aniden" (insert "ani" (insert "abla" empty))
--output: False

--input: search "ani" (insert "abla" (insert "ani" empty))
--output: True
search :: Word -> Trie -> Bool
search [] (Trie b c) = b
search (w:ws) (Trie _ c) = fromMaybe False (fmap (search ws) (M.lookup w c))


getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
