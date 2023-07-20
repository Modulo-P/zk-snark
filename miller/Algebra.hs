{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra where

import           Prelude hiding (divMod, mod, replicate, negate, (-), (+), (*))
import qualified Prelude as P   (negate, (-), (+), (*))

import Params
import AlgebraPlutus

infix  4 ===
infixl 7 \*


-- Class Declarations --

-- | The 'EuclideanRing' class
class Ring a => EuclideanRing a where
  divModE  :: a -> a -> (a, a)
  mod      :: a -> a -> a
  euclides :: a -> a -> (a, a, a)


-- | The 'Field' class
class (Eq a, EuclideanRing a) => Field a where
  embed   :: Integer -> a
  mod0    :: a -> a
  (===)   :: a -> a -> Bool  -- Congruence
  (\*)    :: a -> a -> a     -- Field multiplication
  inverse :: a -> a


-- Integers --

instance EuclideanRing Integer where
  divModE = divMod      -- replace with 'PlutusTx.Numeric.divMod'
  
  mod     = modInteger  -- replace with 'PlutusTx.Builtins.modInteger'
  
  euclides x y = if r < 0 then (-r, -s, -t) else (r, s, t)
    where
      (r, s, t) = go (x, 1, 0) (y, 0, 1) 
      go (r1, s1, t1) (0, _, _)    = (r1, s1, t1)
      go (r0, s0, t0) (r1, s1, t1) =
        let
          (q, r2) = divModE r0 r1
          s2      = s0 - q * s1
          t2      = t0 - q * t1
        in
          go (r1, s1, t1) (r2, s2, t2)


instance Field Integer where
  embed     = id

  mod0 n    = mod n q0

  (===) n m = mod0 (n - m) == 0

  (\*) n m  = mod0 $ n * m

  inverse n = if mod0 n == 0
    then error "tried to divide by zero"
    else mod0 t
         where
           (_, _, t) = euclides q0 n


-- Polynomials --

-- | The "polynomials"
newtype Poly = Poly [Integer]
  deriving (Eq, Show)

instance AdditiveSemigroup Poly where
  (+) (Poly ps) (Poly qs) = Poly $ zipWith (P.+) ps qs

instance AdditiveMonoid Poly where
  zero = Poly $ replicate (k0 P.+ 1) 0

instance AdditiveGroup Poly where
  (-) (Poly ps) (Poly qs) = Poly $ zipWith (P.-) ps qs

instance MultiplicativeSemigroup Poly where
  (*) (Poly ps) (Poly qs) = Poly $ term ps qs <$> [1 .. length' ps]
    where
      term :: [Integer] -> [Integer] -> Integer -> Integer
      term ps qs n = sum $ zipWith (P.*) (take' n ps) (reverse $ take' n qs)

instance MultiplicativeMonoid Poly where
  one = Poly $ [1] <> (replicate k0 0)

-- | "fit" adjusts 'poly' to a list of length 'k0 + 1'
fit :: Poly -> Poly
fit (Poly ps) = Poly $ ps <> (replicate (k0 - length' ps + 1) 0)

-- | Embed into elliptic curve over field extension
embedEC :: EllipticCurve Integer -> EllipticCurve Poly
embedEC (EC n m) = EC (embed n) (embed m)

-- | 'degree' gives the degree of 'poly' in field of "integers mod q0"
degree :: Poly -> Integer
degree (Poly [_]) = 0
degree (Poly ps)  = case mod0 (last ps) of  -- in Plutus code, import 'last' from Prelude
  0 -> degree $ Poly (take' (length' ps - 1) ps)
  _ -> length' ps - 1


-- | Note that 'divModE' and 'euclides' can only be applied to "equal-length"
-- polynomials
instance EuclideanRing Poly where
  divModE n@(Poly ns) d = go (d, zero, n)
    where
      len = length' ns
      go :: (Poly, Poly, Poly) -> (Poly, Poly)
      go (d@(Poly ds), q, r@(Poly rs))
        | deg_r == 0 && (mod0 $ head rs) == 0 = (q, zero)
        | deg_r < deg_d = (q, r)
        | otherwise = let
               t1 = replicate (deg_r - deg_d) 0
               r2 = rs !! (fromIntegral $ deg_r)
               d2 = ds !! (fromIntegral $ deg_d)
               t2 = [r2 \* (inverse d2)]
               t3 = replicate (len - deg_r + deg_d - 1) 0
               t  = Poly $ t1 <> t2 <> t3
            in
               go (d, q + t, r - t * d)
        where
          deg_d = degree d
          deg_r = degree r

  mod x y = snd $ divModE x y

  euclides x y = (pr, ps, pt)
    where
      [pr, ps, pt] = Poly . map (i0 \*) <$> [rs, ss, ts]
      i0 = inverse . head $ rs
      (Poly rs, Poly ss, Poly ts) = go (x, one, zero) (y, zero, one)
      go :: (Poly, Poly, Poly) -> (Poly, Poly, Poly) -> (Poly, Poly, Poly)
      go (r0, s0, t0) (r1, s1, t1)
        | r1 == zero = (r0, s0, t0)
        | otherwise  = let
              (q, r2) = divModE r0 r1
              s2      = s0 - q * s1
              t2      = t0 - q * t1
            in
              go (r1, s1, t1) (r2, s2, t2)
  

-- | Note that '(\*)' can only be applied to "fitted" polynomials
instance Field Poly where
  embed n = fit $ Poly [n]

  mod0 p  = Poly $ mod0 <$> ps
    where
      Poly ps = mod p (Poly poly0)

  (===) p q = mod (p - q) (Poly poly0) == zero

  (\*) (Poly xs) (Poly ys) = Poly $ mod0 <$> zs
    where
      tailZeroes = replicate k0 0
      x = Poly $ xs <> tailZeroes
      y = Poly $ ys <> tailZeroes
      o = Poly $ poly0 <> tailZeroes
      Poly zs' = mod (x * y) o
      zs = take' (k0 + 1) zs'

  inverse x = mod t p0
    where
      p0 = Poly poly0
      (_, _, t) = euclides p0 x


-- Elliptic Curves --

data Field a => EllipticCurve a = EC a a | Infty
  deriving Show

instance Field a => Eq (EllipticCurve a) where
  (==) (EC x1 y1) (EC x2 y2) = mod0 (x2 - x1) == zero && mod0 (y2 - y1) == zero
  (==) (EC _ _) Infty        = False
  (==) Infty (EC _ _)        = False
  (==) Infty Infty           = True

instance Field a => Semigroup (EllipticCurve a) where
  (<>) Infty p = p
  (<>) p Infty = p
  (<>) (EC x1 y1) (EC x2 y2)
    | x1 === x2 && y1 === (negate y2) = Infty
    | x1 === x2 && y1 === y2          = let
           (x, y) = (x1, y1)
           w  = (embed 3) \* (x \* x) \* (inverse $ embed 2 \* y)
           x' = w \* w - embed 2 \* x
           y' = w \* (x - x') - y
       in
           EC (mod0 x') (mod0 y')
    | not (x1 === x2) = let
           w  = (y2 - y1) \* inverse (x2 - x1)
           x3 = w \* w - x1 - x2
           y3 = w \* (x1 - x3) - y1
       in
           EC (mod0 x3) (mod0 y3)


-- Pairing --

-- | From binary representation '[b0, b1, b2, ..., bt]'
fromBinary :: [Integer] -> Integer
fromBinary bits = fst $ foldl (\(a, d) bit -> (a + d * bit, 2 * d)) (0, 1) bits

-- | Checks number matches binary representation
checkBinary :: [Integer] -> Bool
checkBinary bits = last bits == 1 && r_EC == fromBinary bits

-- | Miller's algorithm
miller :: Field a => [Integer] -> EllipticCurve a -> EllipticCurve a -> a
miller bits p q
  | p == Infty || q == Infty || p == q = embed r_sign
  | otherwise = millerGeneric bits p q

-- | Miller's algorithm: generic case
millerGeneric :: Field a => [Integer] -> EllipticCurve a -> EllipticCurve a -> a
millerGeneric bits (EC xP yP) (EC xQ yQ) = g1 \* (inverse g2)
  where
    g1                 = g1' \* (xQ - xT)
    (g1', g2, xT, _yT) = foldr comb (one, one, xP, yP) bits'
    comb               = millerComb (EC xP yP) (EC xQ yQ)
    bits'              = take' (length' bits - 1) bits
    
-- | Accumulator function for Miller's algorithm
millerComb :: Field a => EllipticCurve a -> EllipticCurve a
                         -> Integer -> (a, a, a, a) -> (a, a, a, a)
millerComb (EC xP yP) (EC xQ yQ) b (f1, f2, x, y) = 
  let m   = ((embed 3) \* x \* x) \* (inverse ((embed 2) \* y))
      f1' = f1 \* f1 \* (yQ - y - m \* (xQ - x))
      f2' = f2 \* f2 \* (xQ + (embed 2) \* x - m \* m)
      x'  = m \* m - (embed 2) \* x
      y'  = negate y - m \* (x' - x)
  in  case b of
    0 -> (f1', f2', x', y')
    1 -> let m'   = (y' - yP) \* (inverse (x' - xP))
             f1'' = f1' \* (yQ - y' - m' \* (xQ - x'))
             f2'' = f2' \* (xQ + (xP + x') - m' \* m')
             x''  = m' \* m' - x' - xP
             y''  = negate y' - m' \* (x'' - x')
         in  (f1'', f2'', x'', y'')

-- | Pairing function
pairing :: [Integer] -> EllipticCurve Integer -> EllipticCurve Poly -> Poly
pairing bits p1 q2 = (embed r_sign) \* f_P_Q \* (inverse f_Q_P)
  where
    f_P_Q   = miller' (embedEC p1) q2
    f_Q_P   = miller' q2 (embedEC p1)
    miller' = miller bits


-- Helper functions --

replicate :: Integer -> a -> [a]
replicate n x
  | n <= 0    = []
  | otherwise = x : (replicate (n-1) x)

-- In Plutus code, replace "length'" with "length"
length' :: Foldable t => t a -> Integer
length' = fromIntegral . length

-- In Plutus code, replace "take''" with "take"
take' :: Integer -> [a] -> [a]
take' n = take . fromIntegral $ n
