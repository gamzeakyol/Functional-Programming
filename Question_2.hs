sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 
	where
		sundays' :: Integer -> Integer -> Integer
		sundays' y m
			| y > end = rest 
			| otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
			where
				nextY = y+1
				nextM = m+1
				rest = 0
