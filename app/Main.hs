module Main (main) where

import Lib
import RIO

data Connection = Connection

class HasConnection a where
  getConnection :: a -> Connection

instance HasConnection Connection where
  getConnection = id

class UserRepository m where
  getUserById :: Int -> m Int

instance (HasConnection env) => UserRepository (RIO env) where
  getUserById _ = do
    conn <- asks getConnection
    undefined

main :: IO ()
main = someFunc
