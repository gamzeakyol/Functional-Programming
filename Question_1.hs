
import Prelude

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d  
	| m <= 2 = (d + t4 + k + t2 + t3 + (5 * j)) `mod` 7 
	| otherwise = (d + t1 + k + t2 + t3 + (5 * j)) `mod` 7
	where 
		n = m+12
		j = div y 100 
		k = y `mod` 100
		t1 = div (13*(m+1)) 5
		t2 = div k 4
		t3 = div j 4
		t4 = div (13*(n+1)) 5
	
