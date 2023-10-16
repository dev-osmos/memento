module Effectful.Transaction where

import Control.Lens (Iso', from, view, (^.))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, raise, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, reinterpret, send)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.Error.Dynamic qualified as Eff (Error, throwError)
import Effectful.State.Dynamic qualified as Eff (evalStateShared, get, modify)

data Transaction e :: Effect where
  Abort {-CallStack ->-} :: e -> Transaction e m a
  AddAbortHandler :: (e -> m ()) -> Transaction e m ()

type instance DispatchOf (Transaction e) = 'Dynamic

abort :: forall e a r. (HasCallStack, Transaction e :> r) => e -> Eff r a
abort = send . Abort

addAbortHandler :: (Transaction e :> r) => (e -> Eff r ()) -> Eff r ()
addAbortHandler = send . AddAbortHandler

runTransaction :: forall e a r. (Eff.Error e :> r) => Eff (Transaction e : r) a -> Eff r a
runTransaction = reinterpret (Eff.evalStateShared @[e -> Eff r ()] mempty) \localEnv -> \case
  Abort e -> do
    Eff.get >>= raise . traverse_ \h -> h e
    Eff.throwError e
  AddAbortHandler h -> localSeqUnlift localEnv \unlift -> do
    -- NOTE: @Eff.evalStateShared mempty@ is here for type correctness, need to get rid of it
    let h' = Eff.evalStateShared mempty . unlift . h
    Eff.modify (h' :)

-- | Add handler and run action
(~!!) :: (Transaction e :> r) => Eff (Eff.Error e : r) a -> (e -> Eff r ()) -> Eff r a
act ~!! handler = do
  addAbortHandler handler
  runErrorNoCallStack act >>= either abort pure

-- | Run an action and add handler on success (convenience operator)
(!!~) :: (Transaction e :> r) => Eff (Eff.Error e : r) a -> (e -> Eff r ()) -> Eff r a
act !!~ handler = do
  r <- runErrorNoCallStack act >>= either abort pure
  addAbortHandler handler
  pure r

mapAbort :: Transaction e' :> r => Iso' e e' -> Eff (Transaction e : r) a -> Eff r a
mapAbort i = interpret \localEnv -> \case
  Abort e -> abort $ e ^. i
  AddAbortHandler h -> localSeqUnlift localEnv \unlift -> do
    addAbortHandler $ unlift . h . view (from i)

-- (<!!) :: (HasCallStack, Transaction e :> r) => Maybe a -> e -> Eff r a
(<!!) :: (HasCallStack, Transaction Text :> r) => Maybe a -> Text -> Eff r a
Just x <!! _ = pure x
Nothing <!! e = abort e

-- (.!!) :: (HasCallStack, Transaction e :> r) => (a -> Maybe b) -> e -> a -> Eff r b
(.!!) :: (HasCallStack, Transaction Text :> r) => (a -> Maybe b) -> Text -> a -> Eff r b
f .!! e = (<!! e) . f

infix 5 <!!, .!!
