{-# HLINT ignore "Use list comprehension" #-}
-- @typeName@
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Aeson.Local (CompositeTags (CompositeTags), defaultOptions) where

import Data.Aeson (FromJSON (parseJSON), GFromJSON, GToJSON', Options (..), SumEncoding (..), ToJSON (toJSON), Value (Array, Object), Zero, genericParseJSON, genericToJSON, withObject, (.:), (.=))
import Data.Aeson qualified as A (defaultOptions)
import GHC.Generics (D1, Meta (MetaData), Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Casing (quietSnake)

defaultOptions :: Options
defaultOptions =
  A.defaultOptions
    { constructorTagModifier = quietSnake
    , sumEncoding = error "Use CompositeTags"
    , tagSingleConstructors = False
    }

newtype CompositeTags a = CompositeTags a

compositeTagsOptions :: Options
compositeTagsOptions = defaultOptions {sumEncoding = TwoElemArray, tagSingleConstructors = True, allNullaryToStringTag = False}

instance (Generic a, GFromJSON Zero (Rep a), Rep a ~ D1 ('MetaData name m p nt) x, KnownSymbol name) => FromJSON (CompositeTags a) where
  parseJSON = withObject "CompositeTags a" \obj -> do
    let typeKey = quietSnake (typeName (Proxy @a)) <> ":type"
    t <- obj .: fromString typeKey
    let rest = if length obj > 1 then Object obj else Array mempty
    fmap CompositeTags $ genericParseJSON compositeTagsOptions $ Array [t, rest]

instance (Generic a, GToJSON' Value Zero (Rep a), Rep a ~ D1 ('MetaData name m p nt) x, KnownSymbol name) => ToJSON (CompositeTags a) where
  toJSON (CompositeTags s) =
    let typeKey = quietSnake (typeName (Proxy @a)) <> ":type"
     in case genericToJSON compositeTagsOptions s of
          Array [t, Object v] -> Object $ fromString typeKey .= t <> v
          -- Nullary constructor
          Array [t, Array []] -> Object $ fromString typeKey .= t
          Array [_, Array _] -> error $ "CompositeTags.toJSON: while encoding `" <> toText typeKey <> "`: contents of constructor is not a record, but an array"
          other -> error $ "CompositeTags.toJSON: unreachable code: " <> show other

typeName :: forall a name m p nt x proxy. (Rep a ~ D1 ('MetaData name m p nt) x, KnownSymbol name) => proxy a -> String
typeName _ = symbolVal (Proxy @name)
