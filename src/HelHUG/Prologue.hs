{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HelHUG.Prologue where

import Control.Applicative
import Data.List           (intercalate, sort)
import Data.Maybe          (catMaybes)
import Data.Traversable    (traverse)
import HelHUG.DB
import HelHUG.DB.Attribute

-- | Programming language.
data PL = PL
  { plId         :: EntityId
  , plName       :: String
  , plUrl        :: String
  , plAppearedIn :: Maybe Int
  , plTypingDis  :: [String]
  , plInfluenced :: [EntityId]
  }
  deriving (Eq, Ord, Show, Read)

-- | Conversion from `Entity` to `PL'`. It does resemble aeson parsing...

class IsEntity e where
  fromEntity :: Entity -> Maybe e

instance IsEntity PL where
  fromEntity ent = PL (entityId ent) <$> getAttr "title" ent
                             <*> getAttr "url" ent
                             <*> pure (getAttr "appearedIn" ent)
                             <*> getAttr "typingDiscipline" ent
                             <*> getAttr "influenced" ent






-- We will use doctest (https://github.com/sol/doctest), so the examples are actually runnable!
--
-- $setup
-- >>> Right db <- readDB "pl.json"

-- | I know the entity id of Haskell!
--
-- >>> runDBMonad haskell db
-- Right (Just (PL {plId = EntityId {unEntityId = 314}, plName = "Haskell", ...
haskell :: DBMonad (Maybe PL)
haskell = fromEntity <$> askEntity (EntityId 314)

-- | Let's write more human friendly renderer:
--
-- >>> let Right (Just pl) = runDBMonad haskell db
-- >>> putStrLn $ prettyPL pl
-- name:        Haskell
-- url:         http://en.wikipedia.org/wiki/Haskell_programming_language
-- appeared in: 1990
-- typing:      static, strong, inferred
-- influenced:  [45,50,53,97,108,...
prettyPL :: PL -> String
prettyPL PL {..} = intercalate "\n" [ "name:        " ++ plName
                                    , "url:         " ++ plUrl
                                    , "appeared in: " ++ maybe "-" show plAppearedIn
                                    , "typing:      " ++ intercalate ", " plTypingDis
                                    , "influenced:  " ++ (show . sort . map unEntityId $ plInfluenced)
                                    ]


prettyPL' :: PL -> String -> String
prettyPL' PL {..} influencedStr = intercalate "\n" [ "name:        " ++ plName
                                                   , "url:         " ++ plUrl
                                                   , "appeared in: " ++ maybe "-" show plAppearedIn
                                                   , "typing:      " ++ intercalate ", " plTypingDis
                                                   , "influenced:  " ++ influencedStr
                                                   ]

-- The influenced identifier id's aren't human readable though.
-- Yet to get the information about influenced language, we need to ask for them

-- | Let's write more human friendly renderer:
--
-- >>> let Right (Just pl)  = runDBMonad haskell db
-- >>> let Right str        = runDBMonad (prettierPL pl) db
-- >>> putStrLn str
-- name:        Haskell
-- url:         http://en.wikipedia.org/wiki/Haskell_programming_language
-- appeared in: 1990
-- typing:      static, strong, inferred
-- influenced:  Agda, Bluespec, Inc., C Sharp, C++11, Cayenne, Clean, Clojure, CoffeeScript, ...
prettierPL :: PL -> DBMonad String
prettierPL pl @ PL { plInfluenced = influenced } =
  prettyPL' pl . intercalate ", " . sort . catMaybes <$> traverse askPlName influenced

askPlName :: EntityId -> DBMonad (Maybe String)
askPlName eid = fmap plName . fromEntity <$> askEntity eid

{-
-- But we would like to have `PL -> String` as the type of `prettierPL` too!

-}
