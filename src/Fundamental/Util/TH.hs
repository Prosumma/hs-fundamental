module Fundamental.Util.TH (
    addL,
    addSuffix,
    makeLensesL,
) where

import Control.Lens hiding ((??), (.~), (.=))
import Data.Char
import Language.Haskell.TH
import RIO

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
