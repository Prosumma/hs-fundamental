module Fundamental.Pool
  ( Pool,
    withResource,
  )
where

import Data.Pool (Pool)
import qualified Data.Pool as Pool
import RIO

withResource :: (MonadUnliftIO m) => Pool a -> (a -> m b) -> m b
withResource pool action = withRunInIO $ \runInIO -> Pool.withResource pool (runInIO . action)
