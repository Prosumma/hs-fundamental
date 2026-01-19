module Lib
  ( someFunc,
  )
where

import RIO
import Prelude (putStrLn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
