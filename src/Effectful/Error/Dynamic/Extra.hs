module Effectful.Error.Dynamic.Extra where

import Effectful (Eff, type (:>))
import Effectful.Error.Dynamic qualified as Eff (Error, throwError)

-- (<!) :: (HasCallStack, Eff.Error e :> r) => Maybe a -> e -> Eff r a
(<!) :: (HasCallStack, Eff.Error Text :> r) => Maybe a -> Text -> Eff r a
Just x <! _ = pure x
Nothing <! e = Eff.throwError e

-- (.!) :: (HasCallStack, Eff.Error e :> r) => (a -> Maybe b) -> e -> a -> Eff r b
(.!) :: (HasCallStack, Eff.Error Text :> r) => (a -> Maybe b) -> Text -> a -> Eff r b
f .! e = (<! e) . f

infix 5 <!, .!
