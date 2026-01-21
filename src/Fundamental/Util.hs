{-# LANGUAGE TupleSections #-}

module Fundamental.Util (
  addL,
  addSuffix,
  displayText,
  extractKeys,
  fromTextReader,
  hush,
  makeLensesL,
  makeLensesWith,
  om,
  uformat,
  whenNothing,
  whenNothingM,
  (<#>),
  (<->),
  (<=>),
  (>>=>),
) where

import Control.Lens hiding ((??), (.~), (.=))
import Data.Char
import Data.Either.Extra
import Data.Text.Read
import Data.Foldable
import Formatting
import Language.Haskell.TH
import RIO hiding (Reader)
import RIO.Map (singleton)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified RIO.HashMap as HashMap

hush :: Either e a -> Maybe a
hush = eitherToMaybe

addL :: LensRules
addL = addSuffix "L"

addSuffix ::  String -> LensRules
addSuffix suffix = defaultFieldRules & lensField .~ suffixFieldNamer suffix
  where
    suffixFieldNamer :: String -> FieldNamer
    suffixFieldNamer suffix _ _ field = maybeToList $ do
      let fieldPart = nameBase field
      -- e.g., LensFooL if "L" is the suffix.
      -- This helps disambiguate if DuplicateRecordFields is
      -- also used in the same module.
      let cls = "Lens" ++ capitalize fieldPart ++ suffix
      return (MethodName (mkName cls) (mkName (stripUnderscore fieldPart ++ suffix)))
    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs
    stripUnderscore [] = []
    stripUnderscore s@(c:cs)
      | c == '_' = cs
      | otherwise = s

-- | Makes lenses with an L suffix.
--
-- Use either this or labels.
makeLensesL :: Name -> DecsQ
makeLensesL = makeLensesWith addL

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing cond action = maybe action pure cond

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM cond action = cond >>= maybe action return

fromTextReader :: Reader a -> Text -> Maybe a
fromTextReader reader text = hush (reader text) <&> fst

-- | Shortcut to create a pair.
(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

infixl 8 <->

-- | Shortcut to create a pair in a list
--
-- Useful for creating maps:
--
-- > Map.fromList $ "foo" <#> "bar" <> "bing" <#> "bang"
(<#>) :: a -> b -> [(a, b)]
a <#> b = [a <-> b]

infixl 7 <#>

-- | Shortcut to create a Map from a pair
--
-- Useful for creating maps:
-- > "foo" <=> "bar" <> "bing" <=> "bang"
(<=>) :: k -> v -> Map k v
k <=> v = singleton k v

infixl 7 <=>

-- | The @om@ combinator from `Control.Monad.Extra`.
-- This is chiefly useful in situations like this:
--
-- > foo :: a -> RIO Bar b
-- > foo a = do
-- >   bar <- ask
-- >   baz bar a
--
-- Instead, we can say
--
-- > foo :: a -> RIO Bar b
-- > foo = om bar ask
--
-- However, in most cases it's easier to use @>>=>@,
-- the flipped, infix version of @om@.
om :: Monad m => (a -> b -> m c) -> m a -> b -> m c
om f a = (a >>=) . flip f

-- | Flipped version of 'om' as an operator.
--
-- This is chiefly useful in situations like this:
--
-- > foo :: a -> RIO Bar b
-- > foo a = do
-- >   bar <- ask
-- >   baz bar a
--
-- Instead, we can say
--
-- > foo :: a -> RIO Bar b
-- > foo = ask >>=> baz
(>>=>) :: Monad m => m a -> (a -> b -> m c) -> b -> m c
(>>=>) = flip om

infixl 1 >>=>

displayText :: Text -> Utf8Builder
displayText = display

uformat :: Format Utf8Builder a -> a
uformat m = runFormat m (display . T.toStrict . T.toLazyText)

extractKeys :: Hashable k => Set k -> HashMap k v -> HashMap k v
extractKeys keys hm = HashMap.fromList . mapMaybe extract $ toList keys
  where
    extract k = (k,) <$> HashMap.lookup k hm
