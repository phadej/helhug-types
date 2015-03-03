{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | In this module we describe https://hackage.haskell.org/package/singletons ideas
-- The library was originally presented in Dependently Typed Programming with Singletons,
-- published at the Haskell Symposium, 2012. (http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf)
module HelHUG.Part2 where

import Data.Proxy

-- Natural number
data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show, Read)

nats :: [Nat]
nats = iterate Succ Zero

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

-- Length indexed vector list
data Vec :: * -> Nat -> * where
  Nil  :: forall (a :: *).            Vec a 'Zero
  Cons :: forall (a :: *) (n :: Nat). a -> Vec a n -> Vec a ('Succ n)

vlength :: Vec a n -> Nat
vlength Nil        = Zero
vlength (Cons _ t) = Succ (vlength t)

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ Nil        = Nil
vmap f (Cons h t) = Cons (f h) (vmap f t) -- try with vmap f h -- awesome error

vzipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
vzipWith _ Nil Nil = Nil
vzipWith f (Cons a as) (Cons b bs) = Cons (f a b) (vzipWith f as bs)
-- Error, https://ghc.haskell.org/trac/ghc/ticket/4139 ?
-- vzipWith _ Nil (Cons _ _) = error "can't happen"
-- Will be probably fixed in GHC 7.12: http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/
vzipWith _ _ _ = error "impossibru"

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons h t) = h : toList t

instance Show a => Show (Vec a n) where
  show  = show . toList

-- Singletons
data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: forall (n :: Nat). SNat n -> SNat ('Succ n)

vrepeat :: a -> SNat n -> Vec a n
vrepeat _ SZero        = Nil
vrepeat x (SSucc sn')  = Cons x (vrepeat x sn')

{-
-- | A concrete, promotable proxy type, for use at the kind level
-- There are no instances for this because it is intended at the kind level only
data KProxy (t :: *) = KProxy
-}

-- Dynamic, i.e. value -> type
data SomeSNat :: KProxy Nat -> * where
  SomeSNat :: SNat (n :: Nat) -> SomeSNat ('KProxy :: KProxy Nat)

toSNat :: kparam ~ 'KProxy => Nat -> SomeSNat (kparam :: KProxy Nat)
toSNat Zero = SomeSNat SZero
toSNat (Succ n) = case toSNat n of
                    SomeSNat sn -> SomeSNat (SSucc sn)

-- | Example
--
-- >>> example Zero
-- ""
--
-- >>> example (nats !! 10)
-- "xxxxxxxxxx"
example :: Nat -> String
example n = case toSNat n of
              SomeSNat sn -> toList . vrepeat 'x' $ sn

withSNat :: Nat -> (forall (n :: Nat). SNat n -> b) -> b
withSNat n f = case toSNat n of
                 SomeSNat sn -> f sn

-- | Second example
--
-- >>> example2 Zero
-- ""
--
-- >>> example2 (nats !! 10)
-- "xxxxxxxxxx"
example2 :: Nat -> String
example2 n = withSNat n (toList . vrepeat 'x')

-- | Compile time checked vector lengths:
--
-- >>> withSNat (nats !! 3) (\sn -> toList (vzipWith (,) (vrepeat True sn) (vrepeat 'x' sn))) :: [(Bool, Char)]
-- [(True,'x'),(True,'x'),(True,'x')]
--
-- >>> withSNat (nats !! 3) (\sn -> toList (vzipWith (,) (vrepeat True sn) (vrepeat 'x' (SSucc sn)))) :: [(Bool, Char)]
-- ...
--     Couldn't match type ...
-- ...
