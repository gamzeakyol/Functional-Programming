sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 
	where
		sundays' :: Integer -> Integer -> Integer
		sundays' y m
			| y > end = rest 
			| otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
			where
				nextY = sundays' y+1 m
				nextM = ?
				rest = 0