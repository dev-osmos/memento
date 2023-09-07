{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.History where

import Amazonka (ISO8601)
import Control.Lens ((%~), (^..), (^?), _2, _head, _last)
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
  , descendants :: Vector (ISO8601, StaticVersion)
  -- ^ Path through versions until current
  -- @ISO8601@ corresponds to switch time
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Versions in chronological order: starting with genesis and ending with current
versions :: HistoryGraph -> Vector StaticVersion
versions HistoryGraph {genesis, descendants} = genesis `Vector.cons` fmap snd descendants

contains :: StaticVersion -> HistoryGraph -> Bool
contains v HistoryGraph {genesis, descendants} = genesis == v || Vector.any ((== v) . snd) (Vector.reverse descendants)

currentVersion :: HistoryGraph -> StaticVersion
currentVersion HistoryGraph {genesis, descendants} = fromMaybe genesis $ descendants ^? _last . _2

-- | What versions did this version was switched to?
childrenOf :: StaticVersion -> HistoryGraph -> [(ISO8601, StaticVersion)]
childrenOf v HistoryGraph {genesis, descendants} =
  childrenFromDescendants ((error "unreachable expression", genesis) : Vector.toList descendants)
  where
    childrenFromDescendants [] = []
    childrenFromDescendants ((_, x) : xs) = memptyIfFalse (v == x) (xs ^.. _head) <> childrenFromDescendants xs

versionActiveTimeIntervals :: StaticVersion -> HistoryGraph -> [TimeInterval]
versionActiveTimeIntervals v HistoryGraph {genesis, descendants} =
  intervalsFromDescendants ((Infinite, genesis) : (first Finite <$> Vector.toList descendants))
  where
    intervalsFromDescendants [] = []
    intervalsFromDescendants [(from, x)] = memptyIfFalse (v == x) [TimeInterval {from, until = Infinite}]
    intervalsFromDescendants ((from, x) : xs@((until, _) : _)) = memptyIfFalse (v == x) [TimeInterval {from, until}] <> intervalsFromDescendants xs

data TimeInterval = TimeInterval {from, until :: IntervalBound ISO8601}

data IntervalBound a
  = Finite a
  | Infinite

makeLenses ''HistoryDoc
makeLenses ''HistoryGraph

switch :: ISO8601 -> StaticVersion -> HistoryGraph -> (Bool, HistoryGraph)
switch switchTime newVersion history = (isRollback, newHistory)
  where
    newHistory = history & descendantsL %~ flip Vector.snoc (switchTime, newVersion)
    isRollback = history & contains newVersion
