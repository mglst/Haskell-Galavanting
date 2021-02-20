{-# LANGUAGE InstanceSigs #-}
import Naturals 

data Integers = Diff Naturals Naturals

(+) :: Integers -> Integers -> Integers
(+) (Diff a c) (Diff b d) = Diff (add a b) (add c d)


instance Eq Integers where
    (==) :: Integers -> Integers -> Bool
    (==) (Diff a b) (Diff c d) = add a d == add b c

instance Show Integers where
    show n = show (fromEnum n)
    -- show (Diff a Zero) = show a
    -- show (Diff Zero b) = '-' : show b
    -- show (Diff (Suc a) (Suc b)) = show (Diff a b)


instance Enum Integers where
    toEnum n 
        | n >= 0    = Diff (toEnum n) Zero
        | otherwise = Diff Zero (toEnum (-n))
    
    fromEnum (Diff a b) = (Prelude.-) (fromEnum a) (fromEnum b)

(-) :: Integers -> Integers -> Integers
(-) (Diff a b) (Diff c d) = Diff (add a d) (add b c)


n :: Int -> Integers
n x = toEnum x

(*) :: Integers -> Integers -> Integers
(*) (Diff a b) (Diff c d) = Diff (add (mul a c) (mul b d)) (add (mul b c) (mul a d))