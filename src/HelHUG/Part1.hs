{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module HelHUG.Part1 where

-- One approach would be to define:
--
-- > data PL' = PL'
-- >  { plId' :: EntityId
-- >  , ...
-- >  , plInfluenced' :: [PL]
-- >  }
--
-- And maybe some:
--
-- > class PLClass pl where
-- >   plId :: pl -> EntityId
-- >   ...
--
-- TL;DR so fun, much boilerplate, many loc, wow

import Control.Applicative
import Control.Monad.Error (throwError)
import Data.List           (intercalate, nub, sort)
import Data.Traversable    (traverse)
import HelHUG.DB
import HelHUG.DB.Attribute

-- | We could use DataKinds...
data Zero
data Succ a

newtype Reference (e :: * -> *) = Reference { unReference :: EntityId }

type family Ref n (e :: * -> *) :: * where
  Ref Zero e = Reference e
  Ref (Succ n) e = e n

data PL n = PL
  { plId         :: EntityId
  , plName       :: String
  , plUrl        :: String
  , plAppearedIn :: Maybe Int
  , plTypingDis  :: [String]
  , plInfluenced :: [Ref n PL]
  }

toPL :: Entity -> Maybe (PL Zero)
toPL ent = PL (entityId ent) <$> getAttr "title" ent
                             <*> getAttr "url" ent
                             <*> pure (getAttr "appearedIn" ent)
                             <*> getAttr "typingDiscipline" ent
                             <*> (fmap Reference <$> getAttr "influenced" ent)

-- $setup
-- >>> Right db <- readDB "pl.json"

mlEntityId :: EntityId
mlEntityId = EntityId 89

-- | Let's try ML
-- >>> fmap plName <$> runDBMonad ml db
-- Right (Just "ML")
ml :: DBMonad (Maybe (PL Zero))
ml = toPL <$> askEntity mlEntityId

-- And we can port what we had:
prettyPL' :: PL n -> String -> String
prettyPL' PL {..} influencedStr =
   intercalate "\n" [ "name:        " ++ plName
                    , "url:         " ++ plUrl
                    , "appeared in: " ++ maybe "-" show plAppearedIn
                    , "typing:      " ++ intercalate ", " plTypingDis
                    , "influenced:  " ++ influencedStr
                    ]

-- | And we can continue with what we had:
--
-- >>> let Right (Just pl) = runDBMonad ml db
-- >>> putStrLn $ prettyPL pl
-- name:        ML
-- url:         http://en.wikipedia.org/wiki/ML_(programming_language)
-- appeared in: 1973
-- typing:      static, strong, inferred, safe
-- influenced:  [88,98,105,108,117,122,185,277,287,288]
prettyPL :: PL Zero -> String
prettyPL pl @ PL { plInfluenced = influenced } =
  prettyPL' pl . show . sort . map (unEntityId . unReference) $ influenced

-- Let's write some magic!
class IsEntity (e :: * -> *) n where
  unwrap :: Ref n e -> DBMonad (e n)

instance IsEntity PL Zero where
  unwrap (Reference eid) = do
    entity <- askEntity eid
    case toPL entity of
      Just pl -> return pl
      Nothing -> throwError $ "can't parse entity as PL -- " ++ show eid

instance IsEntity PL n => IsEntity PL (Succ n) where
  unwrap pl = mk <$> traverse unwrap (plInfluenced pl)
    where mk influenced = pl { plInfluenced = influenced }

-- | Let's write more human friendly renderer:
--
-- >>> let Right (Just pl)  = runDBMonad ml db
-- >>> let Right pl'        = runDBMonad (unwrap pl) db
-- >>> putStrLn $ prettierPL pl'
-- name:        ML
-- url:         http://en.wikipedia.org/wiki/ML_(programming_language)
-- appeared in: 1973
-- typing:      static, strong, inferred, safe
-- influenced:  C++, Clojure, Cyclone, Erlang, F Sharp, Felix, Haskell, Miranda, Opa, Scala
prettierPL :: PL (Succ Zero) -> String
prettierPL pl @ PL { plInfluenced = influenced } =
  prettyPL' pl . intercalate ", " . sort . map plName $ influenced

-- | And we can generalise:
--
-- >>> let Right (Just pl)  = runDBMonad ml db
-- >>> let Right pl'        = runDBMonad (unwrap pl) db
-- >>> putStrLn $ prettierPL' pl'
-- name:        ML
-- url:         http://en.wikipedia.org/wiki/ML_(programming_language)
-- appeared in: 1973
-- typing:      static, strong, inferred, safe
-- influenced:  C++, Clojure, Cyclone, Erlang, F Sharp, Felix, Haskell, Miranda, Opa, Scala
prettierPL' :: PL (Succ n) -> String
prettierPL' pl @ PL { plInfluenced = influenced } =
  prettyPL' pl . intercalate ", " . sort . map plName $ influenced

-- So now we could go deeper into the hierarchy:
mlUnwrappedTwice :: DBMonad (PL (Succ (Succ Zero)))
mlUnwrappedTwice = return (Reference mlEntityId) >>= unwrap >>= unwrap >>= unwrap

-- | But we could apply `prettierPL'` still
--
-- >>> let Right pl = runDBMonad mlUnwrappedTwice db
-- >>> putStrLn $ prettierPL' pl
-- name:        ML
-- url:         http://en.wikipedia.org/wiki/ML_(programming_language)
-- appeared in: 1973
-- typing:      static, strong, inferred, safe
-- influenced:  C++, Clojure, Cyclone, Erlang, F Sharp, Felix, Haskell, Miranda, Opa, Scala

-- | An we can get influenced influenced:
--
-- >>> runDBMonad (Data.List.nub . sort . map plName . concatMap plInfluenced . plInfluenced <$> mlUnwrappedTwice) db
-- Right ["Ada","Agda","Akka (toolkit)","Bluespec, Inc.","C Sharp","C++11","C99",...

-- | GHC is smart enough to infer types for complicated expressions like that:
--
-- >>> :t \pl -> Data.List.nub . sort . map plName . concatMap plInfluenced . plInfluenced $ pl
-- \pl -> Data.List.nub . sort . map plName . concatMap plInfluenced . plInfluenced $ pl
--   :: (Ref n2 PL ~ PL n1, Ref n1 PL ~ PL n) => PL n2 -> [String]

magic :: PL (Succ (Succ n)) -> [String]
magic pl = Data.List.nub . sort . map plName . concatMap plInfluenced . plInfluenced $ pl


-- | Error case!
--
-- >>> runDBMonad (prettierPL' <$> return (Reference mlEntityId) >>= unwrap) db
-- ...
--     Couldn't match expected type ...
-- ...
