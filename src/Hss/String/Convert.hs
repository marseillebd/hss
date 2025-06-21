module Hss.String.Convert
  ( IntoOsStr(..)
  , unsafeDecodeUtf
  , default IntoOsStr
  ) where

import Control.Exception (throw)
import Hss.String.Types
import Prelude (id, Either(..), ($), (.))
import System.IO (utf8)
import System.OsPath (encodeWith, decodeWith)
import System.OsPath.Encoding(utf16le_b)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as Sys

unsafeDecodeUtf :: OsPath -> Sys.FilePath
unsafeDecodeUtf ospath = case decodeWith utf8 utf16le_b ospath of
  Right ok -> ok
  Left exn -> throw exn

class IntoOsStr str where
  toOsStr :: str -> OsString
  toPath :: str -> OsPath
  toPath = toOsStr
default IntoOsStr (OsString)

instance IntoOsStr OsPath where
  toOsStr = id

instance IntoOsStr ByteString where
  toOsStr str = case encodeWith utf8 utf16le_b (T.unpack . T.decodeUtf8 $ str) of
    Right ok -> ok
    Left exn -> throw exn

