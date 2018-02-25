
import Prelude

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d  
	| m <= 2 = (d + t4 + k + t2 + t3 + (5 * j)) `mod` 7 
	| otherwise = (d + t1 + k + t2 + t3 + (5 * j)) `mod` 7
	where 
		n = m+12
		j = floor (y/100) 
		k = y `mod` 100
		t1 = floor (fromIntegral (13*(m+1))/5.0)
		t2 = floor (k/4)
		t3 = (floor (y/100)) / 4.0
		t4 = floor (fromIntegral (13*(n+1))/5.0)
	
