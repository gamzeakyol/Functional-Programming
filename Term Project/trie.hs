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


--getWords :: Trie -> [Word]
--getWords trie = traverse trie []
--	where
--		traverse :: Trie -> [Word] -> [Word]
--		traverse trie@(Trie True c) acc = acc
--		traverse trie@(Trie False c) acc = traverse (snd (children trie)) ((fst c)++acc)

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

--I/O operations
--main :: IO ()
--main = do 
--    putStrLn "a) Add Word"
--    putStrLn "s) Search Word"
--    putStrLn "f) Find words with prefix"
--    putStrLn "p) Print all words"
--    putStrLn "e) Exit"
--    putStrLn "Enter the action:"
--    line <- getInput
--    let trie = empty
--    if line == "a"
--        then do putStrLn "Enter word/prefix:"
--                word <- getLine
--				  insert word trie
--                putStrLn "\nNew word is added!"
--                else if line == "s"
--                    then do putStrLn "Enter word/prefix:"
--                            word <- getLine
--                            let result = search word trie
--                            if result == True
--                                then putStrLn "\nExists in dictionary!"
--                                else putStrLn "\nNOT exist!"
--                            else if line == "e"
--                                then return()
