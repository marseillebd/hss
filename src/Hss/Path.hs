module Hss.Path
  ( OsPath, OsString
  , OsChar
  , (</>), (<.>), (-<.>)
  , dirname, basename
  , withCd
  , withTempDir
  ) where

import Hss.String.Types (OsString, OsPath)
import Hss.String.Convert
import Hss.String.Types

import Control.Exception (try, bracket)
import Data.Word (Word32)
import Prelude (Bool(..))
import Prelude (Either(..), IO, Applicative(..), Monad(..), show, (.)) -- DELME
import System.IO.Error (ioError, isAlreadyExistsError)
import System.Random (randomIO)

import System.Directory.OsPath (withCurrentDirectory, removePathForcibly, createDirectory, getTemporaryDirectory)
import System.OsPath (OsChar, (<.>), (-<.>), unsafeEncodeUtf, takeDirectory, takeBaseName)
import Hss.String.Convert

import qualified System.OsPath as Sys
default IntoOsStr (OsString)

(</>) :: (IntoOsStr a, IntoOsStr b) => a -> b -> OsPath
a </> b = toPath a Sys.</> toPath b

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
