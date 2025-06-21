module Hss.String.Types
  ( -- * Blessed String Types
    Bytes
  , Text
  , OsStr
  , Path
  -- * Haskell Strings
  , HsString
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import System.OsPath (OsString, OsPath, unsafeEncodeUtf)
import Data.String (IsString(..))

import qualified Prelude

type Bytes = ByteString

type OsStr = OsString
type Path = OsPath

type HsString = Prelude.String
{-# DEPRECATED HsString "use Text or any of the other un-slow string types" #-}

instance IsString OsStr where
  fromString = unsafeEncodeUtf
