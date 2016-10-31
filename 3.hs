data Date = Date {
	day :: Int,
	month :: Int,
	year :: Int
} deriving(Show, Read)

addDate :: Int -> Int -> Int -> Date

addDate 	d m y | d < 0 || d > 31 = error "Inavlid day"
				  | m `notElem` [01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12] = error "Invalid month"
				  | d > 30 && m `elem` [02, 04, 06, 09, 11] = error "Invalid day for this month"
				  | d > 29 && m == 02 = error "Invalid day for this month" -- still a need to check for leap years
				  | y < 1000 || y > 9999 = error "Invalid year"
				  | otherwise = Date d m y


showd :: Date -> String

showd d = "Date: " ++ show(day d) ++ "/" ++ show(month d) ++ "/" ++ show(year d)