data Date = Date {
	day :: Int,
	month :: Int,
	year :: Int
} deriving(Show, Read)

leapCheck :: Int -> Bool

leapCheck y | (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0) = True

addDate :: Int -> Int -> Int -> Date

addDate d m y | d < 0 || d > 31 = error "Inavlid day"
			  | m `notElem` [01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12] = error "Invalid month"
		  	  | d > 30 && m `elem` [02, 04, 06, 09, 11] = error "Invalid day for this month"
      		  | d > 29 && m == 02 = error "Invalid day for this month" -- still a need to check for leap years
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


data Form = F1 |F2 |F3 |F4 |F5 |F6 |F7 |F8 |F9 |F10 |F11 |F12 |F13 |F14 |F15 |F16 |F17 |F18 |F19 |F20
		    |F21 |F22 |F23 |F24 |F25 |F26 |F27 |F28 |F29 |F30 |F31 |F32 |F33 |F34 |F35 |F36 |F37 |F38 |F39 |F40 deriving(Show)

data Event = Event{
	date :: Date,
	form :: Form,
	comment :: String
}deriving(Show)
} deriving(Show, Read)

addDate :: Int -> Int -> Int -> Date

addDate d m y | d < 0 || d > 31 = error "Inavlid day"
			  | m `notElem` [01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12] = error "Invalid month"
		  	  | d > 30 && m `elem` [02, 04, 06, 09, 11] = error "Invalid day for this month"
      		  | d > 29 && m == 02 = error "Invalid day for this month" -- still a need to check for leap years
			  | y < 1000 || y > 9999 = error "Invalid year"
			  | otherwise = Date d m y


showd :: Date -> String

showd d = "Date: " ++ show(day d) ++ "/" ++ show(month d) ++ "/" ++ show(year d)

before :: (Date,Date) -> Bool

before (d1, d2) | year d1 < year d2 = True
				| year d1 > year d2 = False
				| month d1 < month d2 = True
				| month d1 > month d2 = False
				| day d1 < day d2 = True
				| day d1 > day d2 = False
				| otherwise = False


data Form = F1 
		   |F2
		   |F3
		   |F4
		   |F5
		   |F6
  deriving(Show)

data Event = Event{
	date :: Date,
	form :: Form,
	comment :: String
}deriving(Show)