module Hss.String
  (
  -- * Text Strings
    Text
  , IntoText(..)
  , putStr, putStrLn
  -- * Byte Strings
  , Bytes
  , IntoBytes(..)
  -- * Os Strings
  , OsStr
  , IntoOsStr(..)
  , Path
  ) where

import Hss.String.Bytes
import Hss.String.Text
import Hss.String.Types
import Hss.String.Convert
