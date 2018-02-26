{-- dayOfWeek function is added to the top of this question as it is called from sundays1 function. --}

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d  
	| m <= 2    = (d + t1' + k' + t2' + t3' + (5 * j')) `mod` 7 
	| otherwise = (d + t1 + k + t2 + t3 + (5 * j)) `mod` 7
	where
		y' = y-1
		m' = m+12
		j  = div y 100 
		k  = y `mod` 100
		j' = div y' 100 
		k' = y' `mod` 100
		t1 = div (13*(m+1)) 5
		t1' = div (13*(m'+1)) 5
		t2 = div k 4
		t2'= div k' 4
		t3 = div j 4
		t3' = div j' 4


sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 
	where
		sundays' :: Integer -> Integer -> Integer
		sundays' y m
			| y > end = 0
			| otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
			where
				nextY = if m == 12 then y+1 else y
				nextM = if m < 12 then m+1 else m-11
				rest = sundays' nextY nextM
			
-- Questions

{-- The helper function sundays' calculates and sums up the total number of sundays in a given range of years. --}

{-- If the rest variable was not used and its expression was used wherever it's needed, 
    then readability of the code was decreased and modification of the code would be difficult. --}  
