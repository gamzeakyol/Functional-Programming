
import Prelude

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d  
	| m <= 2 = (d + floor (fromIntegral(13*(n+1))/5) + (y mod 100) + floor ((y mod 100)/4) + ((floor (y/100)) / 4) + 5 * floor (y/100)) mod 7 
	| otherwise = (d + floor (fromIntegral(13*(m+1))/5) + (y mod 100) + floor ((y mod 100)/4) + ((floor (y/100)) / 4) + 5 * floor (y/100)) mod 7
	where 
		n = m+12
	
