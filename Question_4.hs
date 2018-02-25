leap :: Integer -> Bool 
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y 
	| m == 2 = if leap y then 29 else 28
	| (m == 4) || (m == 6) || (m == 9) || (m == 11) = 30
	| otherwise = 31
