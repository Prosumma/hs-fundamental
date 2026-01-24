{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Util (testUtil) where

import Control.Lens (each, _Just)
import Formatting ((%))
import qualified Formatting as F
import Fundamental.Formatting
import Fundamental.Operators
import Fundamental.TH
import RIO
import RIO.Map (fromList)
import Test.Hspec

data Something = Something
  { x :: !Int,
    xs :: ![Maybe Int]
  }
  deriving (Eq, Show)

makeLensesL ''Something

testUtil :: Spec
testUtil = do
  describe "<#>"
    $ it "stitches together a pair into a list"
    $ do
      let expected = fromList [(2, 3), (4, 5)]
      let tested = fromList $ 2 <#> 3 <> 4 <#> 5
      expected `shouldBe` tested
  describe "<=>"
    $ it "stitches together a pair into a Map"
    $ do
      let expected = fromList [(2, 3), (4, 5)]
      let tested = 2 <=> 3 <> 4 <=> 5
      expected `shouldBe` tested
  describe "makeLensesL" $ do
    it "makes lenses with the suffix L" $ do
      let something = Something 2 [Just 4]
      something ^. xL `shouldBe` 2
      something ^.. xsL . each . _Just `shouldBe` [4]
  describe "uformat"
    $ it "formats a RIO Utf8Builder"
    $ do
      let u = uformat ("The value of " % F.text % " is " % F.int % ".") "three" 3
      utf8BuilderToText u `shouldBe` "The value of three is 3."
