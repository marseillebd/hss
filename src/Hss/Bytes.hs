module Hss.Bytes
  ( ByteString
  , BS.empty
  , BS.singleton
  , BS.append
  , BS.null
  , BS.length
  , BS.elemIndex
  , BS.take
  , BS.drop
  , BS.isPrefixOf
  , BS.hGetContents
  , BS.hPut
  , BS8.lines
  ) where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
