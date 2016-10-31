data List a = Nil | Cons a (List a)

hd :: List a -> a
hd Nil = error "Nil does not have a had"
hd (Cons x _) = x

tl :: List a -> List a
tl Nil = error "Nil does not have a tail"
tl (Cons _ l) = l

data Btree a = Leaf a | L (Btree a) a | R a (Btree a) | LR (Btree a) a (Btree a)

data Nat = Zero | Succ Nat deriving(Show)

addNat :: Nat -> Nat -> Nat
addNat n Zero = n
addNat n (Succ m) = Succ (addNat n m) 

