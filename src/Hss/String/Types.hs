module Hss.String.Types
  ( OsString, OsPath
  , ByteString
  , Text
  , IsString(..)
  , default IsString
  ) where

import System.OsPath (OsString, OsPath)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.String (IsString(..))
import System.OsPath (unsafeEncodeUtf)

instance IsString OsPath where
  fromString = unsafeEncodeUtf
default IsString (Text)

