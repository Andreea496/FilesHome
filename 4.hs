data Date = Date {
	day :: Int,
	month :: Int,
	year :: Int
} deriving(Show, Read)

leapCheck :: Int -> Bool

leapCheck y | (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0) = True
			| otherwise = False

addDate :: Int -> Int -> Int -> Date

addDate d m y | d < 0 || d > 31 = error "Inavlid day"
			  | m `notElem` [01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12] = error "Invalid month"
		  	  | d > 30 && m `elem` [02, 04, 06, 09, 11] = error "Invalid day for this month"
      		  | d > 29 && m == 02 && (leapCheck y == True) = error "Invalid day for this month in leap year" 
      		  | d > 28 && m == 02 && (leapCheck y == False) = error "Invalid day for this month" 
			  | y < 0 = error "Invalid year"
			  | otherwise = Date d m y


showd :: Date -> String

showd d = "Date: " ++ show(day d) ++ "/" ++ show(month d) ++ "/" ++ show(year d)

-- assumption that for equal dates outputs false

before :: (Date,Date) -> Bool
before (d1, d2) | year d1 < year d2 = True
				| (year d1 == year d2) && (month d1 < month d2) = True
				| (year d1 == year d2) && (month d1 == month d2) && (day d1 < day d2) = True
				| otherwise = False
