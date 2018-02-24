
import Prelude

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d  
	| m<=2 = n where n = m+12
		
	j = floor y/100
	k = y 'mod' 100
	
	t1 = floor (13*(m+1)/5)
	t2 = floor k/4
	t3 = floor j/4
	z = (d + t1 + k + t2 + t3 + 5 * j) 'mod' 7
	
	return z
	