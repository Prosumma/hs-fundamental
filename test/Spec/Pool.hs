{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Pool (testPool) where

import Data.Pool (defaultPoolConfig, newPool, setNumStripes)
import Fundamental.Pool
import RIO
import Test.Hspec

-- | Builds a single-stripe pool whose one resource is a shared counter 'IORef'.
newCounterPool :: IORef Int -> IO (Pool (IORef Int))
newCounterPool counter =
  newPool (setNumStripes (Just 1) (defaultPoolConfig (pure counter) (const (pure ())) 10 1))

testPool :: Spec
testPool = do
  describe "withResource" $ do
    it "returns the result produced by the action" $ do
      counter <- newIORef 0
      pool <- newCounterPool counter
      withResource pool (\_ -> pure 42) `shouldReturn` (42 :: Int)

    it "hands the pooled resource to the action" $ do
      counter <- newIORef 0
      pool <- newCounterPool counter
      result <- withResource pool $ \c -> do
        modifyIORef' c (+ 1)
        readIORef c
      result `shouldBe` 1
      readIORef counter `shouldReturn` 1
