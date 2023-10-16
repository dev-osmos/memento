module Effectful.Amazonka where

import Amazonka (AWSPager, AWSRequest, AWSResponse, Env, runResourceT)
import Amazonka qualified as A (Error, paginate, paginateEither, send, sendEither)
import Conduit (ConduitM)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic qualified as Eff (Reader, ask)
import Effectful.Resource (Resource)

send :: (HasCallStack, AWSRequest a, Typeable a, Typeable (AWSResponse a), IOE :> r, Eff.Reader Env :> r) => a -> Eff r (AWSResponse a)
send r = do
  e <- Eff.ask
  runResourceT do
    A.send e r

sendEither :: (HasCallStack, AWSRequest a, Typeable a, Typeable (AWSResponse a), IOE :> r, Eff.Reader Env :> r) => a -> Eff r (Either A.Error (AWSResponse a))
sendEither r = do
  e <- Eff.ask
  runResourceT do
    A.sendEither e r

paginate :: (HasCallStack, AWSPager a, Typeable a, Typeable (AWSResponse a), IOE :> r, Resource :> r, Eff.Reader Env :> r) => a -> ConduitM () (AWSResponse a) (Eff r) ()
paginate r = do
  e <- lift Eff.ask
  A.paginate e r

paginateEither :: (HasCallStack, AWSPager a, Typeable a, Typeable (AWSResponse a), IOE :> r, Resource :> r, Eff.Reader Env :> r) => a -> ConduitM () (AWSResponse a) (Eff r) (Either A.Error ())
paginateEither r = do
  e <- lift Eff.ask
  A.paginateEither e r
