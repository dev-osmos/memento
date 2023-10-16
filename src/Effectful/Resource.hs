{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Resource where

import Conduit (ResourceT, runResourceT)
import Control.Monad.Trans.Resource (MonadResource (liftResourceT))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data Resource :: Effect where
  LiftResourceT :: ResourceT IO a -> Resource m a

type instance DispatchOf Resource = 'Dynamic

instance (Resource :> r, IOE :> r) => MonadResource (Eff r) where
  liftResourceT = send . LiftResourceT

runResource :: IOE :> r => Eff (Resource : r) a -> Eff r a
runResource = interpret \_localEs -> \case
  LiftResourceT r -> liftIO $ runResourceT r
