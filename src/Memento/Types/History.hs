{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.History where

import Amazonka (ISO8601)
import Control.Lens (view, (%~), (^..), (^?), _head, _last)
import Control.Lens.Local (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (any, cons, reverse, snoc, toList)
import Memento.Types.Static (StaticVersion)

newtype HistoryDoc = HistoryDoc
  { history :: HistoryGraph
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

initHistory :: StaticVersion -> HistoryDoc
initHistory genesis = HistoryDoc {history = HistoryGraph {genesis, descendants = mempty}}

data HistoryGraph = HistoryGraph
  { genesis :: StaticVersion
  -- ^ The very first version of this static
  , descendants :: Vector Descendant
  -- ^ Path through versions until current
  -- @ISO8601@ corresponds to switch time
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Descendant = Descendant
  { switchTime :: ISO8601
  , version :: StaticVersion
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeLenses ''HistoryDoc
makeLenses ''HistoryGraph
makeLenses ''Descendant

-- | Versions in chronological order: starting with genesis and ending with current
versions :: HistoryGraph -> Vector StaticVersion
versions HistoryGraph {genesis, descendants} = genesis `Vector.cons` fmap (view versionL) descendants

contains :: StaticVersion -> HistoryGraph -> Bool
contains v HistoryGraph {genesis, descendants} = genesis == v || Vector.any ((== v) . view versionL) (Vector.reverse descendants)

currentVersion :: HistoryGraph -> StaticVersion
currentVersion HistoryGraph {genesis, descendants} = fromMaybe genesis $ descendants ^? _last . versionL

-- | What versions did this version was switched to?
childrenOf :: StaticVersion -> HistoryGraph -> [Descendant]
childrenOf v HistoryGraph {genesis, descendants} =
  childrenFromDescendants (Descendant {switchTime = error "unreachable expression", version = genesis} : Vector.toList descendants)
  where
    childrenFromDescendants [] = []
    childrenFromDescendants (Descendant {version} : xs) = memptyIfFalse (v == version) (xs ^.. _head) <> childrenFromDescendants xs

versionActiveTimeIntervals :: StaticVersion -> HistoryGraph -> [TimeInterval]
versionActiveTimeIntervals v HistoryGraph {genesis, descendants} =
  intervalsFromDescendants $ (Infinite, genesis) : (Vector.toList descendants <&> \Descendant {switchTime, version} -> (Finite switchTime, version))
  where
    intervalsFromDescendants [] = []
    intervalsFromDescendants [(from, x)] = memptyIfFalse (v == x) [TimeInterval {from, until = Infinite}]
    intervalsFromDescendants ((from, x) : xs@((until, _) : _)) = memptyIfFalse (v == x) [TimeInterval {from, until}] <> intervalsFromDescendants xs

data TimeInterval = TimeInterval {from, until :: IntervalBound ISO8601}

data IntervalBound a
  = Finite a
  | Infinite

switch :: ISO8601 -> StaticVersion -> HistoryGraph -> (Bool, HistoryGraph)
switch switchTime newVersion history = (isRollback, newHistory)
  where
    newHistory = history & descendantsL %~ flip Vector.snoc Descendant {switchTime, version = newVersion}
    isRollback = history & contains newVersion
