module Effectful.Break where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data Break a :: Effect where
  Break :: a -> Break a m a

type instance DispatchOf (Break a) = 'Dynamic

break :: (Break a :> r) => a -> Eff r a
break = send . Break

runBreak :: Eff (Break a : r) a -> Eff r a
runBreak = interpret \_localEnv (Break x) -> pure x
