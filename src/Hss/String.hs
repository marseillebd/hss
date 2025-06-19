module Hss.String
  ( IsString(..)
  , StringLike(..)
  ) where

import Prelude ()
import qualified Prelude

import Data.String (IsString)
import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy as LBS

class IsString str => StringLike str where
  empty :: str
  toLazyText :: str -> Text

instance StringLike ByteString where
  empty = LBS.empty
  toLazyText = decodeUtf8
