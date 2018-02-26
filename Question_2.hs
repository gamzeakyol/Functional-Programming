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
