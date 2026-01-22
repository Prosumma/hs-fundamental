module Fundamental.Control (
    hush,
    om,
    whenNothing,
    whenNothingM,
    (>>=>),
) where

import Data.Either.Extra
import RIO

hush :: Either e a -> Maybe a
hush = eitherToMaybe

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing cond action = maybe action pure cond

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM cond action = cond >>= maybe action return

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
