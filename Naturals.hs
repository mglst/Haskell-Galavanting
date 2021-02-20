{-# LANGUAGE InstanceSigs #-}
module Naturals where

data Naturals = Zero | Suc Naturals

instance Eq Naturals where
        (==) :: Naturals -> Naturals -> Bool
        (==) Zero Zero = True
        (==) (Suc a) (Suc b) = a == b
        (==) _ _ = False

instance Enum Naturals where
        toEnum 0 = Zero
        toEnum n = Suc (toEnum (n-1))

        fromEnum Zero = 0
        fromEnum (Suc n) = (fromEnum n) + 1

instance Show Naturals where
        show n = show (fromEnum n)

add :: Naturals -> Naturals -> Naturals
add n Zero = n
add n (Suc m) = Suc (add n m)

mul :: Naturals -> Naturals -> Naturals
mul m Zero = Zero
mul m (Suc n) = add (mul m n) m

expp :: Naturals -> Naturals -> Naturals
expp (Suc m) Zero = Suc Zero
expp m (Suc n) = mul (expp m n) m

z :: Int -> Naturals
z n = toEnum n

