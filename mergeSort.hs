 msort :: (Ord a) => [a] -> [a] -> [a]
 msort xs [] = xs
 msort [] ys = ys
 msort (x:xs) (y:ys) | (x <= y)  = x:(msort xs (y:ys)) 
          		 	 | otherwise = y:(msort (x:xs) ys)
