{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Note: We need `UndecidableInstances` to only derive `Eq`. We *know* it will be safe.

-- | This is essentially `Part3b` with additional methods
module HelHUG.Part3b where

------------------------------------------------------------------------
-- Library code
--

import           Control.Applicative
import           Control.Monad.Error (throwError)
import           Data.List (sort, intercalate)
import           Data.Traversable
import qualified HelHUG.DB as DB (entityId)
import           HelHUG.DB hiding (entityId)
import           HelHUG.DB.Attribute

-- | Peano numbers.
data Nat = Zero | Succ Nat

-- | Singleton type for `Nat`
data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: forall (n :: Nat). SNat n -> SNat ('Succ n)

-- | Implicit construction of `SNat` from type-level `Nat`.
class INat (n :: Nat) where
  snat :: SNat n

instance INat 'Zero where
  snat = SZero

instance INat n => INat (Succ n) where
  snat = SSucc snat

-- Four eye smilies!
infixr 5 :<:, :>:

-- | In absense of better name: @HList@.
data HList :: Nat -> [Either * (Nat -> *)] -> * where
  HNil  :: HList n '[]
  (:<:) :: a -> HList n as -> HList n (Left a ': as)
  (:>:) :: IsEntity e => [Ref n e] -> HList n as -> HList n (Right e ': as)

newtype Reference (e :: Nat -> *) = Reference { unReference :: EntityId }
  deriving (Eq, Ord, Show, Read)

type family Ref (n :: Nat) (e :: Nat -> *) :: * where
  Ref Zero e = Reference e
  Ref (Succ n) e = e n

-- | Like @foldr (->) b as@, but on the type-level
type family HApply (n :: Nat) (as :: [Either * (Nat -> *)]) (b :: *) :: * where
  HApply n '[] b = b
  HApply n (Left a ': as) b = a -> HApply n as b
  HApply n (Right a ': as) b = [Ref n a] -> HApply n as b

-- | I'm kind of surprised this works!
hApply :: HApply n spec b -> HList n spec -> b
hApply f HNil       = f
hApply f (a :<: as) = hApply (f a) as
hApply f (a :>: as) = hApply (f a) as

-- Class definitions:

-- | `IsRecord` is isomorphic with some `HList`.
class IsRecord (e :: Nat -> *) where
  type Spec e :: [Either * (Nat -> *)]
  toHList :: e n -> HList n (Spec e)
  fromHList :: HList n (Spec e) -> e n

-- | `IsEntity` is familiar class
class IsEntity (e :: Nat -> *) where
  toEntity :: Entity -> Maybe (e Zero)
  entityId :: e n -> EntityId

  unwrapExpl :: SNat n -> Ref n e -> DBMonad (e n)
  default unwrapExpl :: IsRecord e => SNat n -> Ref n e -> DBMonad (e n)
  unwrapExpl SZero      ref  = unwrapReference ref
  unwrapExpl (SSucc sn) ent  = fromHList <$> unwrapHList sn (toHList ent)

  wrapExpl :: SNat n -> e n -> Ref n e
  default wrapExpl :: IsRecord e => SNat n -> e n -> Ref n e
  wrapExpl SZero      ent  = Reference (entityId ent)
  wrapExpl (SSucc sn) ent  = fromHList . wrapHList sn . toHList $ ent

unwrap :: (IsEntity e, INat n) => Ref n e -> DBMonad (e n)
unwrap = unwrapExpl snat

wrap :: (IsEntity e, INat n) => e n -> Ref n e
wrap = wrapExpl snat

-- Unwrapping helpers

unwrapReference :: IsEntity e => Reference e -> DBMonad (e Zero)
unwrapReference (Reference eid) = do
  entity <- askEntity eid
  case toEntity entity of
    Just pl -> return pl
    Nothing -> throwError $ "can't parse entity -- " ++ show eid

unwrapHList :: SNat n -> HList n spec -> DBMonad (HList (Succ n) spec)
unwrapHList _   HNil      = pure HNil
unwrapHList sn (x :<: xs) = (x :<:) <$> unwrapHList sn xs
unwrapHList sn (e :>: xs) = (:>:) <$> traverse (unwrapExpl sn) e <*> unwrapHList sn xs

wrapHList :: SNat n -> HList (Succ n) spec -> HList n spec
wrapHList _ HNil        = HNil
wrapHList sn (x :<: xs) = x :<: wrapHList sn xs
wrapHList sn (e :>: xs) = map (wrapExpl sn) e :>: wrapHList sn xs

------------------------------------------------------------------------
-- User code:
--

data PL (n :: Nat) = PL
  { plId :: EntityId
  , plName :: String
  , plUrl :: String
  , plAppearedIn :: Maybe Int
  , plTypingDis  :: [String]
  , plInfluenced :: [Ref n PL]
  }

deriving instance Eq (Ref n PL) => Eq (PL n)

-- | Structure of `PL`.
--
-- >>> :kind PLSpec
-- PLSpec :: [Either * (Nat -> *)]
type PLSpec = '[ Left EntityId, Left String, Left String, Left (Maybe Int), Left [String], Right PL ]

instance IsRecord PL where
  type Spec PL = PLSpec

  -- This is the only boilerplate code.
  -- Unfortunately, it cannot be derivied using Generics, at least elegantly.
  -- But should be not hard using Template Haskell
  toHList PL {..} = plId :<: plName :<: plUrl :<: plAppearedIn :<: plTypingDis :<: plInfluenced :>: HNil
  fromHList hlist = hApply PL hlist

instance IsEntity PL where
  entityId = plId
  toEntity ent = PL (DB.entityId ent) <$> getAttr "title" ent
                                      <*> getAttr "url" ent
                                      <*> pure (getAttr "appearedIn" ent)
                                      <*> getAttr "typingDiscipline" ent
                                      <*> (fmap Reference <$> getAttr "influenced" ent)

------------------------------------------------------------------------
-- Test code
--

-- $setup
-- >>> Right db <- readDB "pl.json"

cEntityId :: EntityId
cEntityId = EntityId 32

-- | Let's try ML
-- >>> fmap plName <$> runDBMonad clang db
-- Right (Just "C")
clang :: DBMonad (Maybe (PL Zero))
clang = toEntity <$> askEntity cEntityId

prettyPL' :: PL n -> String -> String
prettyPL' PL {..} influencedStr =
   intercalate "\n" [ "name:        " ++ plName
                    , "url:         " ++ plUrl
                    , "appeared in: " ++ maybe "-" show plAppearedIn
                    , "typing:      " ++ intercalate ", " plTypingDis
                    , "influenced:  " ++ influencedStr
                    ]

prettierPL :: PL (Succ n) -> String
prettierPL pl @ PL { plInfluenced = influenced } =
  prettyPL' pl . intercalate ", " . sort . map plName $ influenced

cUnwrappedTwice :: DBMonad (PL (Succ (Succ Zero)))
cUnwrappedTwice = return (Reference cEntityId) >>= unwrap >>= unwrap >>= unwrap

-- | Let's try!
--
-- >>> let Right pl = runDBMonad cUnwrappedTwice db
-- >>> putStrLn $ prettierPL pl
-- name:        C
-- url:         http://en.wikipedia.org/wiki/C_(programming_language)
-- appeared in: 1972
-- typing:      static, weak, manifest, nominal
-- influenced:  AMPL, AWK, BitC, C Sharp, ...

-- | Error case!
--
-- >>> runDBMonad (prettierPL <$> return (Reference cEntityId) >>= unwrap) db
-- ...
--     Couldn't match expected type ...
-- ...

-- | And `wrap`
--
-- >>> let Right pl = runDBMonad (wrap <$> cUnwrappedTwice) db
-- >>> putStrLn $ prettierPL pl
-- name:        C
-- url:         http://en.wikipedia.org/wiki/C_(programming_language)
-- appeared in: 1972
-- typing:      static, weak, manifest, nominal
-- influenced:  AMPL, AWK, BitC, C Sharp, ...

-- | Equality: `(==)`
--
-- >>> let Right pl = runDBMonad cUnwrappedTwice db
-- >>> pl == pl
-- True
