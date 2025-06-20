module Hss
  (
  -- * Shell
    exe
  , (|>)
  , (&>)
  , (&>>)
  , capture
  -- * Environment
  , getArgs
  , withCd
  , withTempDir
  -- * Basic Data Types
  -- ** Booleans
  , Bool(..)
  -- ** Strings
  , here
  -- ** Paths
  , module Hss.Path
  , toOsPath
  , fromOsPath
  , readFile
  , writeFile
  -- * Structural Data Types
  -- * Composition
  -- * Effects
  -- * Prelude
  ) where

import Prelude hiding (readFile, writeFile)

-- import System.IO (withFile, IOMode(..), openFile)

-- import Hss.Applicative
-- import Hss.Base
-- import Hss.Bool
-- import Hss.Data
-- import Hss.Function
-- import Hss.Functor
-- import Hss.IO
-- import Hss.Monad
import Hss.Path
-- import Hss.String

import Shh (exe, (|>), capture)
import Data.String.Here.Uninterpolated (here)
import System.Environment (getArgs)
import Data.ByteString (ByteString)
import Data.String (fromString)
import System.IO (IOMode(..), withFile, openFile)
import Control.Exception (throw)
import System.OsPath.Encoding(utf16le_b)
import System.IO (utf8)
import System.OsPath (OsPath, encodeWith, decodeWith)

import qualified Data.ByteString as BS
import qualified Shh
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

infixl 9 &>
(&>) :: Shh.Shell m => Shh.Proc a -> OsPath -> m a
p &> fpath = p Shh.&> Shh.Truncate (fromString . unsafeDecodeUtf $ fpath)

infixl 9 &>>
(&>>) :: Shh.Shell m => Shh.Proc a -> OsPath -> m a
p &>> fpath = p Shh.&> Shh.Append (fromString . unsafeDecodeUtf $ fpath)

-- TODO move to an IO module
readFile :: OsPath -> IO ByteString
readFile path = openFile (unsafeDecodeUtf path) ReadMode >>= BS.hGetContents
writeFile :: OsPath -> ByteString -> IO ()
writeFile path content = withFile (unsafeDecodeUtf path) WriteMode $ \fp ->
  BS.hPutStr fp content

toOsPath :: ByteString -> OsPath
toOsPath str = case encodeWith utf8 utf16le_b (T.unpack . T.decodeUtf8 $ str) of
  Right ok -> ok
  Left exn -> throw exn

fromOsPath :: OsPath -> ByteString
fromOsPath str = case decodeWith utf8 utf16le_b str of
  Right ok -> T.encodeUtf8 . T.pack $ ok
  Left exn -> throw exn

instance Shh.ExecArg OsString where
  asArg s = [fromString $ unsafeDecodeUtf s]
