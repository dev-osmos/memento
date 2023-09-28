module Data.TreeJson (TreeJson (TreeJson)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), withObject, (.:), (.=))
import Data.Tree (Tree (..))

newtype TreeJson a = TreeJson (Tree a)

instance (ToJSON a) => ToJSON (TreeJson a) where
  toJSON (TreeJson Node {rootLabel, subForest}) =
    Object $
      "value"
        .= rootLabel
        <> "children"
        .= (TreeJson <$> subForest)

instance (FromJSON a) => FromJSON (TreeJson a) where
  parseJSON = fmap TreeJson . withObject "TreeJson" \obj -> Node <$> obj .: "value" <*> (fmap (coerce @(TreeJson a)) <$> obj .: "children")
