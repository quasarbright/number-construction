module Lib
    ( someFunc
    ) where

import Prelude hiding (succ, div)

-- naturals

class IsNatural a where
  zero :: a
  succ :: a -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data Peano = Zero | Succ Peano deriving (Eq, Show)

instance IsNatural Peano where
  zero = Zero
  succ = Succ
  add Zero b = b
  add (Succ a) b = Succ (add a b)
  mul Zero _ = zero
  mul (Succ a) b = add b (mul a b)

instance Ord Peano where
  _ < Zero = False
  Zero < Succ _ = True
  Succ a < Succ b = a < b
  a <= b = (a < b) || (a == b)

-- integers

class IsNatural a => IsInteger a where
  sub :: a -> a -> a

neg :: IsInteger a => a -> a
neg n = sub zero n

-- Sub a b represents a - b
data Sub nat = Sub nat nat deriving (Show)

instance IsNatural nat => IsNatural (Sub nat) where
  zero = Sub zero zero
  succ (Sub a b) = Sub (succ a) b
  -- a - b + c - d = (a + c) - (b + d)
  add (Sub a b) (Sub c d) = Sub (add a c) (add b d)
  -- (a - b) * (c - d) = (ac + bd) - (bc + ad)
  mul (Sub a b) (Sub c d) = Sub (add (mul a c) (mul b d)) (add (mul b c) (mul a d))

instance IsNatural nat => IsInteger (Sub nat) where
  -- (a - b) - (c - d) = (a + d) - (b + c)
  sub (Sub a b) (Sub c d) = Sub (add a d) (add b c)

instance (Eq nat, IsNatural nat) => Eq (Sub nat) where
  -- a - b = c - d <=> a + d = c + b
  Sub a b == Sub c d = add a d == add c b

instance (Ord nat, IsNatural nat) => Ord (Sub nat) where
  -- a - b <= c - d <=> a + d <= c + b
  Sub a b <= Sub c d = add a d <= add c b

-- rationals

class IsInteger a => IsRational a where
  div :: a -> a -> a

recip :: IsRational a => a -> a
recip x = div (succ zero) x

-- Div p q represents p / q
-- CONSTRAINT: q /= 0
data Div int = Div int int deriving (Show)

instance IsNatural int => IsNatural (Div int) where
  zero = Div zero (succ zero)
  -- 1 + p/q = (p + q) / q
  succ (Div p q) = Div (add p q) q
  -- p/q + r/s = (ps + qr) / (q + s)
  add (Div p q) (Div r s) = Div (add (mul p s) (mul q r)) (add q s)
  -- (p/q) * (r/s) = pq/rs
  mul (Div p q) (Div r s) = Div (mul p r) (mul q s)

instance IsInteger int => IsInteger (Div int) where
  -- p/q - r/s = (ps - qr) / (q + s)
  sub (Div p q) (Div r s) = Div (sub (mul p s) (mul q r)) (add q s)

instance IsInteger int => IsRational (Div int) where
  -- (p/q)/(r/s) = ps/qr
  div (Div p q) (Div r s) = Div (mul p s) (mul q r)

instance (IsNatural int, Eq int) => Eq (Div int) where
  -- p/q = r/s <=> ps = qr
  Div p q == Div r s = mul p s == mul q r

instance (IsNatural int, Ord int) => Ord (Div int) where
  -- p/q <= r/s <=> ps <= qr
  Div p q <= Div r s = mul p s <= mul q r




someFunc :: IO ()
someFunc = putStrLn "someFunc"
