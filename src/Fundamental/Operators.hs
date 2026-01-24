module Fundamental.Operators
  ( (<#>),
    (<=>),
  )
where

import RIO
import RIO.Map (singleton)

-- | Shortcut to create a pair in a list
--
-- Useful for creating maps:
--
-- > Map.fromList $ "foo" <#> "bar" <> "bing" <#> "bang"
(<#>) :: a -> b -> [(a, b)]
a <#> b = [(a, b)]

infixl 7 <#>

-- | Shortcut to create a Map from a pair
--
-- Useful for creating maps:
-- > "foo" <=> "bar" <> "bing" <=> "bang"
(<=>) :: k -> v -> Map k v
k <=> v = singleton k v

infixl 7 <=>
