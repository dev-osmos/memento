{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import Data.Composition ((.:))
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Reader.Dynamic qualified as Eff (Reader (Ask, Local))
import Effectful.State.Dynamic qualified as Eff (State (..))

instance (Eff.Reader s :> r, MonadReader s (Eff r)) => MonadReader s (Eff r) where
  ask = send Eff.Ask
  local = send .: Eff.Local

instance (Eff.State s :> r, MonadState s (Eff r)) => MonadState s (Eff r) where
  get = send Eff.Get
  put = send . Eff.Put
