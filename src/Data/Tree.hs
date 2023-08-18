module Data.Tree where

data Tree node leaf
  = Node { node :: node, children :: NonEmpty (Tree node leaf) }
  | Leaf { leaf :: leaf }

newtype TreePath = TreePath [Int]
