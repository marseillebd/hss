module Hss.Path
  ( OsPath, OsString
  , OsChar
  , osp
  , pack, unpack
  , (</>), (<.>), (-<.>)
  , dirname, basename
  , withCd
  , withTempDir
  , unsafeEncodeUtf, unsafeDecodeUtf
  ) where

import Prelude (Bool(..))
import Prelude (Either(..), IO, Applicative(..), Monad(..), show, (.)) -- DELME
import Data.String (IsString(..))
import System.IO.Error (ioError, isAlreadyExistsError)
import Control.Exception (try, bracket, throw)
import System.Random (randomIO)
import System.IO (utf8, FilePath)
import System.OsPath.Encoding(utf16le_b)

import System.OsPath
import System.Directory.OsPath

import Data.Word (Word32)

dirname :: OsPath -> OsPath
dirname = takeDirectory

basename :: OsPath -> OsString
basename = takeBaseName

-- TODO re-export other path manipulation functions

-- FIXME withTempDir and withCd should go in an Environment module
--FIXME allow adding a template
withTempDir :: (OsPath -> IO a) -> IO a
withTempDir = bracket mkDir rmDir
  where
  mkDir :: IO OsPath
  mkDir = getTemporaryDirectory >>= loop
  loop :: OsPath -> IO OsPath
  loop parentDir = do
    nonce <- randomIO :: IO Word32
    let dirName = parentDir </> "hss" <.> (unsafeEncodeUtf . show) nonce
    try (createDirectory dirName) >>= \case
      Left exn | isAlreadyExistsError exn -> loop parentDir
               | True -> ioError exn
      Right () -> pure dirName
  -- rmDir = const $ pure () -- DEBUG
  rmDir = removePathForcibly

withCd :: OsPath -> IO a -> IO a
withCd = withCurrentDirectory

instance IsString OsPath where
  fromString = unsafeEncodeUtf

unsafeDecodeUtf :: OsPath -> FilePath
unsafeDecodeUtf ospath = case decodeWith utf8 utf16le_b ospath of
  Right ok -> ok
  Left exn -> throw exn
