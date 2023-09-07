module Effectful.Transaction where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, raise, (:>))
import Effectful.Dispatch.Dynamic (localSeqUnlift, reinterpret, send)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.Error.Dynamic qualified as Eff (Error, throwError)
import Effectful.State.Dynamic qualified as Eff (evalStateShared, get, modify)

data Transaction e :: Effect where
  Abort :: e -> Transaction e m a
  AddAbortHandler :: (e -> m ()) -> Transaction e m ()

type instance DispatchOf (Transaction e) = 'Dynamic

abort :: forall e a r. (Transaction e :> r) => e -> Eff r a
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

(~!!) :: (Transaction e :> r) => Eff (Eff.Error e : r) a -> (e -> Eff r ()) -> Eff r a
act ~!! handler = do
  addAbortHandler handler
  runErrorNoCallStack act >>= either abort pure
