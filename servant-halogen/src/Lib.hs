module Lib where

import HaskellSay (haskellSay)
import qualified System.IO as IO

hi :: IO ()
hi = haskellSay "Hello Haskell Nixers!"
