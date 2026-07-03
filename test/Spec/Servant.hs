module Spec.Servant (testServant) where

import Fundamental.Servant
import RIO
import Servant (ServerError, err404, err500)
import System.IO.Error (userError)
import Test.Hspec

testServant :: Spec
testServant = do
  describe "mapServerException" $ do
    it "passes a ServerError through unchanged" $
      mapServerException (toException err404) `shouldBe` (Left err404 :: ServerResponse ())
    it "maps any other exception to err500" $
      mapServerException (toException (userError "boom")) `shouldBe` (Left err500 :: ServerResponse ())

  describe "defaultExceptionHandler" $
    it "maps an exception within RIO" $
      runRIO () (defaultExceptionHandler (toException err404))
        `shouldReturn` (Left err404 :: ServerResponse ())

  describe "maybeThrow404" $ do
    it "returns the value when the action yields Just" $
      maybeThrow404 (pure (Just 5)) `shouldReturn` (5 :: Int)
    it "throws err404 when the action yields Nothing" $
      (maybeThrow404 (pure Nothing) :: IO Int) `shouldThrow` (== (err404 :: ServerError))
