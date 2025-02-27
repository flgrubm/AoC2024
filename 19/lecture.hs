s :: Int -> Int
s = \n -> n + 1

plus0 :: Int -> Int -> Int
plus0 x 0 = x
plus0 x y = (plus0 x (y-1)) + 1

data Nat = Z | S Nat deriving (Show)

rec :: a -> (Nat -> a -> a) -> Nat -> a
rec dZ dS Z = dZ
rec dZ dS (S n') = dS n' (rec dZ dS n')

plus :: Nat -> Nat -> Nat
plus = \x y -> rec x (\ z w -> S w) y

ackInt :: Int -> Int -> Int
ackInt 0 n = n + 1
ackInt m 0 = ackInt (m-1) 1
ackInt m n = ackInt (m-1) (ackInt m (n-1))
