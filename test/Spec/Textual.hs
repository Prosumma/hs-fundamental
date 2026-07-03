{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Textual (testTextual) where

import Data.Aeson.Types (Value (..), parseMaybe)
import qualified Data.Attoparsec.Text as Atto
import Fundamental.Textual
import RIO
import Test.Hspec

-- | A minimal newtype whose @Read@/@Show@ are derived from 'Textual'.
newtype Wrapped = Wrapped Int deriving (Eq)

instance FromText Wrapped where
  fromText = fmap Wrapped . fromText

instance ToText Wrapped where
  toText (Wrapped n) = toText n

instance Show Wrapped where
  show = showTextual

instance Read Wrapped where
  readPrec = readTextual

testTextual :: Spec
testTextual = do
  describe "FromText / ToText round trips" $ do
    it "handles Text" $ do
      fromText "hello" `shouldBe` Just ("hello" :: Text)
      toText ("hello" :: Text) `shouldBe` "hello"
    it "handles ByteString" $ do
      fromText "hello" `shouldBe` Just ("hello" :: ByteString)
      toText ("hello" :: ByteString) `shouldBe` "hello"
    it "handles Int" $ do
      fromText "42" `shouldBe` Just (42 :: Int)
      toText (42 :: Int) `shouldBe` "42"
    it "handles Integer" $ do
      fromText "42" `shouldBe` Just (42 :: Integer)
      toText (42 :: Integer) `shouldBe` "42"
    it "handles Double" $ do
      fromText "3.5" `shouldBe` Just (3.5 :: Double)
      toText (3.5 :: Double) `shouldBe` "3.5"
    it "returns Nothing when a number cannot be parsed" $
      (fromText "nope" :: Maybe Int) `shouldBe` Nothing

  describe "unsafeFromText" $
    it "extracts the value when the text is valid" $
      unsafeFromText "42" `shouldBe` (42 :: Int)

  describe "parseTextual" $ do
    it "parses a value that consumes all input" $
      parseTextual (Atto.string "hi") "hi" `shouldBe` Just "hi"
    it "fails when input remains after the parser" $
      parseTextual (Atto.string "hi") "hiya" `shouldBe` Nothing

  describe "parseJSONTextual" $ do
    it "parses a JSON string into a FromText value" $
      parseMaybe (parseJSONTextual "Int") (String "42") `shouldBe` Just (42 :: Int)
    it "fails on an invalid JSON string" $
      parseMaybe (parseJSONTextual "Int") (String "nope") `shouldBe` (Nothing :: Maybe Int)
    it "fails on a non-string JSON value" $
      parseMaybe (parseJSONTextual "Int") (Bool True) `shouldBe` (Nothing :: Maybe Int)

  describe "toJSONTextual" $
    it "renders a ToText value as a JSON string" $
      toJSONTextual (42 :: Int) `shouldBe` String "42"

  describe "fromStringTextual" $
    it "builds a value from a String when valid" $
      fromStringTextual "Int" "42" `shouldBe` (42 :: Int)

  describe "showTextual" $
    it "shows a value via ToText" $
      showTextual (42 :: Int) `shouldBe` "42"

  describe "parseUrlPieceTextual" $ do
    it "parses a valid piece" $
      parseUrlPieceTextual "Int" "42" `shouldBe` (Right 42 :: Either Text Int)
    it "reports a helpful error for an invalid piece" $
      (parseUrlPieceTextual "Int" "nope" :: Either Text Int)
        `shouldBe` Left "'nope' is not a valid Int."

  describe "Textual-derived Read/Show" $ do
    it "shows via showTextual" $
      show (Wrapped 7) `shouldBe` "7"
    it "reads via readTextual" $
      readMaybe "7" `shouldBe` Just (Wrapped 7)
    it "fails to read invalid input" $
      (readMaybe "nope" :: Maybe Wrapped) `shouldBe` Nothing
