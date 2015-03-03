{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Helpers to work with `Entity` attributes.
module HelHUG.DB.Attribute where

import Data.Map as Map
import HelHUG.DB

class Attribute a where
  getAttr :: AttrName -> Entity -> Maybe a

instance Attribute String where
  getAttr attrName (Entity _ attrs) = Map.lookup attrName attrs >>= fromAttrStr

instance Attribute [String] where
  getAttr attrName (Entity _ attrs) = Map.lookup attrName attrs >>= fromAttrMultiStr

instance Attribute Int where
  getAttr attrName (Entity _ attrs) = Map.lookup attrName attrs >>= fromAttrInt

instance Attribute [EntityId] where
  getAttr attrName (Entity _ attrs) = Map.lookup attrName attrs >>= fromAttrCollection

fromAttrInt :: AttrValue -> Maybe Int
fromAttrInt (AttrInt i) = Just i
fromAttrInt _           = Nothing

fromAttrStr :: AttrValue -> Maybe String
fromAttrStr (AttrStr str) = Just str
fromAttrStr _             = Nothing

fromAttrMultiStr :: AttrValue -> Maybe [String]
fromAttrMultiStr (AttrMultiStr strs) = Just strs
fromAttrMultiStr (AttrCollection []) = Just []
fromAttrMultiStr _                   = Nothing

fromAttrCollection :: AttrValue -> Maybe [EntityId]
fromAttrCollection (AttrCollection coll) = Just coll
fromAttrCollection (AttrMultiStr [])     = Just []
fromAttrCollection _                     = Nothing
