import Data.Char (isDigit, digitToInt)
import System.IO

cardlist = [Card {suit = Diamonds, rank = Jack}, Card {suit = Hearts, rank = Queen}, Card {suit = Diamonds, rank = Jack}, Card {suit = Hearts, rank = Num 8}]

data Color = Red | Black
             deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace
            deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank }
            deriving (Show, Eq)
data Move = Draw | Discard Card
            deriving (Show, Eq)

-- Question 1
cardColor :: Card -> Color
cardColor card = case card of 
	Card {suit = Clubs}   -> Black 
	Card {suit = Spades}  -> Black 
	_      		      -> Red

-- Question 2
cardValue :: Card -> Int
cardValue card = case card of 
	Card {rank = Num value} 	-> value
	Card {rank = Ace}	        -> 11
	_      				-> 10

-- Question 3
-- raise an error will be added	
removeCard :: [Card] -> Card -> [Card]
removeCard cs c
	| c == head cs = tail cs
	| otherwise    = (head cs) : (removeCard (tail cs) c)
			
-- Question 4
allSameColor :: [Card] -> Bool
allSameColor cs
	| tail cs             == []                        = True
	| cardColor (head cs) /= cardColor (head(tail cs)) = False
	| otherwise 					   = allSameColor (tail cs)

-- Question 5
sumCards :: [Card] -> Int
sumCards cs = sumCardsHelper cs 0
	where 
		sumCardsHelper :: [Card] -> Int -> Int
		sumCardsHelper cs' acc
			| cs' == [] = acc
			| otherwise = sumCardsHelper (tail cs') (cardValue (head cs') + acc)  

-- Question 6
score :: [Card] -> Int -> Int
score cs goal
	| allSameColor cs = (preliminaryResult cs goal) `div` 2
	| otherwise       = preliminaryResult cs goal
		where 
			preliminaryResult :: [Card] -> Int -> Int
			preliminaryResult cs goal
				| sum > goal         = 3 * (sum - goal)
				| otherwise 	     = goal - sum
					where
						sum = sumCards cs
						
-- Question 7
type State = ([Card], [Card], [Move])

-- Question 8
runGame :: [Card] -> [Move] -> Int -> Int
heldcards = []
runGame cardlist movelist goal = runGameHelper (heldcards, cardlist, movelist)
	where
		runGameHelper :: State -> Int
		runGameHelper state = case state of
			(_, _, [])		    -> score heldcards goal
			(_, _, ((Discard card):ms)) -> runGameHelper ((removeCard heldcards card), cardlist, (tail movelist))
			(_, [], (Draw:ms))          -> score heldcards goal
			(_, _, (Draw:ms))           -> if (sumCards heldcards) > goal then (score heldcards goal) else runGameHelper (((head cardlist):heldcards), (tail cardlist), (tail movelist))							
				where
					ms = tail movelist

-- Question 9
convertSuit :: Char -> Suit
convertSuit c
	| c == 'D' || c == 'd' = Diamonds
	| c == 'H' || c == 'h' = Hearts
	| c == 'S' || c == 's' = Spades
	| c == 'C' || c == 'c' = Clubs
	| otherwise = error "The suit is unknown"

-- Question 10
convertRank :: Char -> Rank
convertRank c
	| c == 'J' || c == 'j' = Jack
	| c == 'Q' || c == 'q' = Queen
	| c == 'K' || c == 'k' = King
	| c == '1'             = Ace
	| c == 'T' || c == 't' = Num 10
	| otherwise            = if (isDigit c) then Num (digitToInt c) else error "The rank is unknown"

-- Question 11
convertCard :: Char -> Char -> Card
convertCard s r = Card {suit = convertSuit s, rank = convertRank r}
				
-- Question 12
						  
-- Question 13
convertMove :: Char -> Char -> Char -> Move
convertMove m s r 
	| m == 'd' || m == 'D' = Draw
	| m == 'r' || m == 'R' = Discard (convertCard s r)
	
-- Question 14


-- Question 15

