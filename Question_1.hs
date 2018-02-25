
import Prelude

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
	
