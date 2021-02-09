data Natural = Zero | Suc Natural

instance Enum Natural where
        toEnum 0 = Zero
        toEnum n = Suc (toEnum (n-1))

        fromEnum Zero = 0
        fromEnum (Suc n) = (fromEnum n) + 1

instance Show Natural where
        show n = show (fromEnum n)

add :: Natural -> Natural -> Natural
add n Zero = n
add n (Suc m) = Suc (add n m)

mul :: Natural -> Natural -> Natural
mul m Zero = Zero
mul m (Suc n) = add (mul m n) m

expp :: Natural -> Natural -> Natural
expp (Suc m) Zero = Suc Zero
expp m (Suc n) = mul (expp m n) m

z :: Int -> Natural
z n = toEnum n

isZero :: Natural -> Bool
isZero Zero = True
isZero (Suc _) = False