module Hss.ByteString
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
  ) where

import Prelude --DELME
import Data.Word -- DELME

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.IO as BS


