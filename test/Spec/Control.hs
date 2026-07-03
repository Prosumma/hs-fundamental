{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Control (testControl) where

import Fundamental.Control
import RIO
import Test.Hspec

testControl :: Spec
testControl = do
  describe "hush" $ do
    it "turns Right into Just" $
      hush (Right 5 :: Either String Int) `shouldBe` Just 5
    it "turns Left into Nothing" $
      hush (Left "boom" :: Either String Int) `shouldBe` Nothing

  describe "whenNothing" $ do
    it "returns the wrapped value when Just" $
      whenNothing (Just 5) (pure 0) `shouldReturn` (5 :: Int)
    it "runs the action when Nothing" $
      whenNothing Nothing (pure 0) `shouldReturn` (0 :: Int)

  describe "whenNothingM" $ do
    it "returns the wrapped value when the condition is Just" $
      whenNothingM (pure (Just 5)) (pure 0) `shouldReturn` (5 :: Int)
    it "runs the action when the condition is Nothing" $
      whenNothingM (pure Nothing) (pure 0) `shouldReturn` (0 :: Int)

  describe "om" $
    it "feeds a monadic value in as the first argument" $
      om (\a b -> pure (a + b)) (pure 3) 4 `shouldReturn` (7 :: Int)

  describe ">>=>" $
    it "is the flipped, infix form of om" $
      (pure 3 >>=> \a b -> pure (a + b)) 4 `shouldReturn` (7 :: Int)
