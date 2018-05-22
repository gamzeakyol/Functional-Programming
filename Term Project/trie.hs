import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
type Word = String

empty :: Trie
empty = Trie {end = False, children = M.empty}

insert :: Word -> Trie -> Trie
insert [] (Trie _ c)     = Trie True c
insert (w:ws) (Trie b c) = Trie b newChildren
	where
		newChildren = M.insert w (insert ws child) c
			where 
				child = fromMaybe empty (M.lookup w c)  

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
