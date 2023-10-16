module Effectful.Break where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Error.Static qualified as Eff (runErrorNoCallStack, throwError)

data Break e :: Effect where
  Break :: e -> Break e m a

type instance DispatchOf (Break e) = 'Dynamic

break :: (Break e :> r) => e -> Eff r a
break = send . Break

runBreakHomo :: Eff (Break a : r) a -> Eff r a
runBreakHomo = reinterpret (fmap (either id id) . Eff.runErrorNoCallStack) \_localEnv (Break x) -> Eff.throwError x
