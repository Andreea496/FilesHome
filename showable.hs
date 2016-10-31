--{- Infix show function for List a -}
--write :: Show a => List a -> String
--write l = [ : (f l)
--				where
--				f :: Show a => List a -> String
--				f Nil = "]"
--				f (Cons x Nil) = (show x) ++ "]"
--				f (Cons x l) = (show x) ++ "," ++ (f l)

ord3 :: (Bool3, Bool3) -> Bool3
ord3 (U, ) = T
ord3 (T, T) = T
ord3 (F, F) = T
ord3 = F

data Bool3 = T | F | U

write3 :: Bool3 -> String
write3 x | x == T = show(True)
		 | x == F = show(False)
		 | x == U = show(Unkown)