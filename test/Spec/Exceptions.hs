{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec.Exceptions (testExceptions) where

import Fundamental.Exceptions
import RIO
import Test.Hspec

-- | Stands in for an underlying error, e.g. a database @SqlError@.
newtype OrigE = OrigE Text deriving (Eq, Show)

instance Exception OrigE

-- | Stands in for the error we want to surface, e.g. a Servant @ServerError@.
newtype MappedE = MappedE Int deriving (Eq, Show)

instance Exception MappedE

-- | A record with an @httpStatus@ field, exercising the @HasField@-based helpers.
newtype Response = Response
  { httpStatus :: Int
  }
  deriving (Eq, Show, Generic)

-- | Maps @OrigE "boom"@ onto @MappedE 404@, passing everything else through.
handler :: MatchException OrigE MappedE
handler = throwWhen (\(OrigE t) -> t == "boom") (MappedE 404)

testExceptions :: Spec
testExceptions = do
  describe "isException" $ do
    it "recognizes an exception of the given type" $
      isException (Proxy @OrigE) (toException (OrigE "x")) `shouldBe` True
    it "rejects an exception of a different type" $
      isException (Proxy @OrigE) (toException (MappedE 1)) `shouldBe` False

  describe "throwWhen" $ do
    it "rethrows the matched value when the predicate holds" $
      throwWhen (== (5 :: Int)) "matched" 5 `shouldBe` (Left "matched" :: Either String Int)
    it "passes the original through when the predicate fails" $
      throwWhen (== (5 :: Int)) "matched" 6 `shouldBe` (Right 6 :: Either String Int)

  describe "matchException" $ do
    it "maps a matching exception to the mapped exception" $
      case matchException handler (toException (OrigE "boom")) of
        Left se -> fromException se `shouldBe` Just (MappedE 404)
        Right _ -> expectationFailure "expected a Left"
    it "passes a non-matching exception of the same type through" $
      case matchException handler (toException (OrigE "meh")) of
        Left _ -> expectationFailure "expected a Right"
        Right se -> fromException se `shouldBe` Just (OrigE "meh")
    it "passes an exception of an unrelated type through" $
      case matchException handler (toException (MappedE 7)) of
        Left _ -> expectationFailure "expected a Right"
        Right se -> fromException se `shouldBe` Just (MappedE 7)

  describe "matchAll" $
    it "unconditionally maps to the fallback exception" $
      case matchAll (MappedE 500) (toException (OrigE "anything")) of
        Left se -> fromException se `shouldBe` Just (MappedE 500)
        Right _ -> expectationFailure "expected a Left"

  describe "throwMatch" $
    it "rethrows the mapped exception" $
      throwMatch (matchException handler) (toException (OrigE "boom"))
        `shouldThrow` (== MappedE 404)

  describe "catchMatch" $ do
    it "maps an exception thrown by the action" $
      catchMatch (matchException handler) (throwIO (OrigE "boom") :: IO ())
        `shouldThrow` (== MappedE 404)
    it "rethrows an unmatched exception unchanged" $
      catchMatch (matchException handler) (throwIO (OrigE "meh") :: IO ())
        `shouldThrow` (== OrigE "meh")

  describe "throwOnHttpStatusOutsideRange" $ do
    it "returns the response when the status is in range" $
      throwOnHttpStatusOutsideRange [200 .. 299] HTTPStatusException (Response 204)
        `shouldReturn` Response 204
    it "throws when the status is out of range" $
      (throwOnHttpStatusOutsideRange [200 .. 299] HTTPStatusException (Response 500) :: IO Response)
        `shouldThrow` (== HTTPStatusException 500)

  describe "throwOnHttpStatusError" $ do
    it "returns the response for a 2xx status" $
      throwOnHttpStatusError HTTPStatusException (Response 200)
        `shouldReturn` Response 200
    it "throws for a non-2xx status" $
      (throwOnHttpStatusError HTTPStatusException (Response 404) :: IO Response)
        `shouldThrow` (== HTTPStatusException 404)

  describe "maybeThrowM" $ do
    it "returns the value for Just" $
      maybeThrowM (MappedE 1) (Just 5) `shouldReturn` (5 :: Int)
    it "throws for Nothing" $
      (maybeThrowM (MappedE 1) Nothing :: IO Int) `shouldThrow` (== MappedE 1)

  describe "maybeThrowIO" $ do
    it "returns the value for Just" $
      maybeThrowIO (MappedE 1) (Just 5) `shouldReturn` (5 :: Int)
    it "throws for Nothing" $
      (maybeThrowIO (MappedE 1) Nothing :: IO Int) `shouldThrow` (== MappedE 1)

  describe "maybeThrowError" $ do
    it "returns the value for Just" $
      maybeThrowError (MappedE 1) (Just 5) `shouldBe` (Right 5 :: Either MappedE Int)
    it "throws for Nothing" $
      maybeThrowError (MappedE 1) Nothing `shouldBe` (Left (MappedE 1) :: Either MappedE Int)

  describe "eitherThrowM" $ do
    it "returns the value for Right" $
      eitherThrowM MappedE (Right 5) `shouldReturn` (5 :: Int)
    it "throws the mapped value for Left" $
      (eitherThrowM MappedE (Left 9) :: IO Int) `shouldThrow` (== MappedE 9)

  describe "eitherThrowIO" $ do
    it "returns the value for Right" $
      eitherThrowIO MappedE (Right 5) `shouldReturn` (5 :: Int)
    it "throws the mapped value for Left" $
      (eitherThrowIO MappedE (Left 9) :: IO Int) `shouldThrow` (== MappedE 9)

  describe "eitherThrowError" $ do
    it "returns the value for Right" $
      eitherThrowError MappedE (Right 5) `shouldBe` (Right 5 :: Either MappedE Int)
    it "throws the mapped value for Left" $
      eitherThrowError MappedE (Left 9) `shouldBe` (Left (MappedE 9) :: Either MappedE Int)
