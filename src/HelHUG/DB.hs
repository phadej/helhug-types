{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Entity database is a bit different from document database.
-- Instead of storing documents in key-value fashion (@Map DocumentId Document@), we store attributes of the entities:
--
-- > type DB  = Map EntityId (Map AttrName AttrValue)
-- > type DB' = Map (EntityId, AttrName) AttrValue
--
-- Entity database is somewhere in between NoSQL and SQL databases.
module HelHUG.DB (
  -- * DB types
    EntityId(..)
  , AttrName(..)
  , AttrValue(..)
  , Entity(..)
  , DB(..)
  , readDB
  -- * DB monad
  , DBMonad
  , runDBMonad
  , askEntity
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson
import Data.String
import Data.Aeson.Types
import Data.ByteString.Lazy as BL (readFile)
import Data.HashMap.Lazy as HashMap
import Data.Map as Map
import Data.Text as T
import Data.Traversable (traverse)
import Prelude as P

-- | Entity identifier is an int.
newtype EntityId = EntityId { unEntityId :: Int }
  deriving (Eq, Ord, Show, Read)

-- | Attribute name is a string.
newtype AttrName = AttrName String
  deriving (Eq, Ord, Show, Read)

instance IsString AttrName where
  fromString = AttrName

-- | We have only same attribute types, and related values.
--
-- In our example database is read from JSON file. We carefully picked attribute types, so their JSON representation is unambigious
data AttrValue = AttrStr String            -- ^ represented as string
               | AttrMultiStr [String]     -- ^ represented as array of strings
               | AttrInt Int               -- ^ represented as ints
               | AttrCollection [EntityId] -- ^ represented as array of ints
  deriving (Eq, Ord, Show, Read)

-- | Entity has an identifier and is a collection of attributes.
data Entity = Entity
  { entityId    :: EntityId
  , entityAttrs :: Map AttrName AttrValue
  }
  deriving (Eq, Ord, Show, Read)

-- | DB is a collection of entities.
newtype DB = DB { unDB :: Map EntityId Entity }
  deriving (Eq, Ord, Show, Read)

-- JSON instances:
instance FromJSON EntityId where
  parseJSON v = EntityId <$> parseJSON v

instance FromJSON AttrValue where
  parseJSON v = parseStr <|> parseInt <|> parseCollection <|> parseMultiStr
    where parseStr = AttrStr <$> parseJSON v
          parseInt = AttrInt <$> parseJSON v
          parseCollection = AttrCollection <$> parseJSON v
          parseMultiStr = AttrMultiStr <$> parseJSON v

parseAttrs :: Object -> Parser (Map AttrName AttrValue)
parseAttrs v = Map.fromList <$> traverse parsePair (P.filter keyNotId . HashMap.toList $ v)
  where keyNotId (k, _)   = k /= "id"
        parsePair (k, v') = (AttrName (T.unpack k),) <$> parseJSON v'

instance FromJSON Entity where
  parseJSON (Object v) = Entity <$> v .: "id"
                                <*> parseAttrs v
  parseJSON _ = mzero

makeDB :: [Entity] -> DB
makeDB = DB . Map.fromList . fmap (\e -> (entityId e, e))

instance FromJSON DB where
  parseJSON v = makeDB <$> parseJSON v

-- | Read database from JSON file.
readDB :: FilePath -> IO (Either String DB)
readDB filename = eitherDecode <$> BL.readFile filename

-- | Monad, so it's easy to work with in-memory DB!
--
-- We use `ReaderT` as we won't mutate `DB` in our examples.
type DBMonad = ReaderT DB (ErrorT String Identity)

-- | DBMonad is a pure computation.
runDBMonad :: DBMonad a -> DB -> Either String a
runDBMonad m db = runIdentity $ runErrorT (runReaderT m db)

-- | Ask for the entity.
--
-- In the real world it could make a HTTP request to @"http://datastore.internal/entity/:id"@, for example.
askEntity :: EntityId -> DBMonad Entity
askEntity eid = do
  db <- unDB <$> ask
  case Map.lookup eid db of
    Just e  -> return e
    Nothing -> throwError $ "entity doesn't exist -- id = " ++ show (unEntityId eid)

