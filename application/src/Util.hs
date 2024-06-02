module Util (MonadRunHttp(..)) where

import Control.Monad.Trans
import Network.HTTP.Req

class (MonadIO m) => MonadRunHttp m where
  runHttpReq :: Req a -> m a
  runHttpReq = liftIO . runReq defaultHttpConfig
