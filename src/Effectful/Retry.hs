module Effectful.Retry where

import Colog (Message, Severity (Debug, Warning))
import Data.Vector qualified as Vector
import Effectful (Eff, type (:>))
import Effectful.Colog.Dynamic (Logger, log)
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Transaction (Transaction, abort)

retry :: (HasCallStack, Concurrent :> r, Logger Message :> r) => RetryConfig -> Eff r (Either e a) -> Eff r (RetryResult e a)
retry RetryConfig {times, delayInit, delayModifier} action = withFrozenCallStack (go mempty delayInit)
  where
    go errors delayCurrent =
      action >>= \case
        Right result -> pure RetryOk {errors = Vector.toList errors, result}
        Left err
          | Vector.length errors >= times -> pure $ RetryExhausted $ err :| Vector.toList errors
          | otherwise -> do
              log Debug $ "action returned error, sleeping for " <> show delayCurrent <> "μs"
              threadDelay delayCurrent
              go (Vector.cons err errors) (delayModifier delayCurrent)

retryEither :: (Logger Message :> r, Concurrent :> r) => RetryConfig -> Eff r (Either e a) -> Eff r (Either (NonEmpty e) a)
retryEither c =
  retry c >>> fmap \case
    RetryExhausted es -> Left es
    RetryOk {result} -> Right result

retryTx :: (HasCallStack, Transaction e :> r, Logger Message :> r, Concurrent :> r) => RetryConfig -> (NonEmpty Text -> e) -> Eff r (Either Text a) -> Eff r a
retryTx c f =
  retry c >=> \case
    RetryOk {errors, result} -> do
      for_ errors $ log Warning
      pure result
    RetryExhausted es -> abort $ f es

data RetryConfig = RetryConfig
  { times :: Int
  , delayInit :: Int
  -- ^ microseconds
  , delayModifier :: Int -> Int
  }

defRC :: RetryConfig
defRC = RetryConfig {times = 10, delayInit = 1_000_000, delayModifier = \prev -> 10_000_000 `min` ((3 * prev) `div` 2)}

data RetryResult e a
  = -- | reversed
    RetryExhausted (NonEmpty e)
  | RetryOk
      { errors :: [e]
      -- ^ reversed
      , result :: a
      }
