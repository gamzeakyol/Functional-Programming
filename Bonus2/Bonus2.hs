cardlist = [Card {suit = Diamonds, rank = Jack}, Card {suit = Hearts, rank = Queen}, Card {suit = Spades, rank = Num 10}]

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
	_      			          -> Red
  
-- Question 2
cardValue :: Card -> Int
cardValue card = case card of 
	Card {rank = Ace}			-> 11 
	Card {rank = Num 2} 		-> 2
	Card {rank = Num 3}			-> 3
	Card {rank = Num 4} 		-> 4
	Card {rank = Num 5} 		-> 5
	Card {rank = Num 6} 		-> 6
	Card {rank = Num 7} 		-> 7
	Card {rank = Num 8} 		-> 8
	Card {rank = Num 9} 		-> 9
	_      						      -> 10

-- Question 3
-- raising an error will be added	
removeCard :: [Card] -> Card -> [Card]
removeCard cs c
	| c == head cs = tail cs
	| otherwise    = (head cs) : (removeCard (tail cs) c)
	

