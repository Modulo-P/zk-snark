{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module AlgebraPlutus where

import           Prelude hiding (divMod, mod, (-), (+), (*))
import qualified Prelude as P   (divMod, mod, (-), (+), (*))

import Params

infixl 7 *
infixl 6 +, -


-- Defined in PlutusTx.Builtins --

divMod :: Integer -> Integer -> (Integer, Integer)
divMod = P.divMod

modInteger :: Integer -> Integer -> Integer
modInteger = P.mod


-- Defined in PlutusTx.Monoid --

-- | A 'Group'.
class Monoid a => Group a where
  inv :: a -> a


-- Defined in PlutusTx.Numeric --

-- | A 'Semigroup' that is sensible to describe using addition.
class AdditiveSemigroup a where
  (+) :: a -> a -> a

instance AdditiveSemigroup Integer where
  (+) = (P.+)

-- | A 'Monoid' that is sensible to describe using addition and zero.
class AdditiveSemigroup a => AdditiveMonoid a where
  zero :: a

instance AdditiveMonoid Integer where
  zero = 0

-- | A 'Group' that it is sensible to describe using addition, zero, and subtraction.
class AdditiveMonoid a => AdditiveGroup a where
  (-) :: a -> a -> a

instance AdditiveGroup Integer where
  (-) = (P.-)

negate :: AdditiveGroup a => a -> a
negate x = zero - x

-- | A 'Semigroup' that is sensible to describe using multiplication.
class MultiplicativeSemigroup a where
  (*) :: a -> a -> a

instance MultiplicativeSemigroup Integer where
  (*) = (P.*)

-- | A 'Semigroup' that is sensible to describe using multiplication and one.
class MultiplicativeSemigroup a => MultiplicativeMonoid a where
  one :: a

instance MultiplicativeMonoid Integer where
  one = 1

-- | A ring.
type Ring a = (AdditiveGroup a, MultiplicativeMonoid a)

-- | A module, with a type of scalars which can be used to scale the values.
class (Ring s, AdditiveGroup v) => Module s v | v -> s where
-- class (Ring s, AdditiveGroup v) => Module s v where
    scale :: s -> v -> v
