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
	_      		      -> Red
  
-- Question 2
cardValue :: Card -> Int
cardValue card = case card of 
	Card {rank = Ace}	        -> 11 
	Card {rank = Num 2} 		-> 2
	Card {rank = Num 3}	        -> 3
	Card {rank = Num 4} 		-> 4
	Card {rank = Num 5} 		-> 5
	Card {rank = Num 6} 		-> 6
	Card {rank = Num 7} 		-> 7
	Card {rank = Num 8} 		-> 8
	Card {rank = Num 9} 		-> 9
	_      			        -> 10

-- Question 3
-- raising an error will be added	
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
				| otherwise          = goal - sum
					where
						sum = sumCards cs
						
-- Question 7
data State = HeldCards [Card] | CardList [Card]

