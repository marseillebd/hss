module Hss.String.Text
  ( Text
  , T.empty
  , T.singleton
  , T.append
  , T.null
  , T.length
  , T.take
  , T.drop
  , T.isPrefixOf
  , T.hGetContents
  , T.hPutStr
  , T.lines
  , T.putStr, T.putStrLn
  ) where

import Hss.String.Types (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T
