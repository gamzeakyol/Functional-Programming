leap :: Integer -> Bool 
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y 
	| m == 2 = if leap y then 29 else 28
	| (m == 4) || (m == 6) || (m == 9) || (m == 11) = 30
	| otherwise = 31
	
sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 2
	where
		sundays' :: Integer -> Integer -> Integer -> Integer
		sundays' y m weekday
			| y > end = 0
			| otherwise = if nextWeekday `mod` 7 == 0 then rest + 1 else rest						
			where
				nextY = if m == 12 then y+1 else y
				nextM = if m < 12 then m+1 else m-11
				nextWeekday = weekday + ((daysInMonth m y) `mod` 7) 
				rest = sundays' nextY nextM nextWeekday
