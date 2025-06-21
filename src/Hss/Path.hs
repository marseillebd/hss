module Hss.Path
  ( Path
  , (</>), (<.>), (-<.>)
  , dirname, basename
  , withCd
  , withTempDir
  ) where

import Hss.Preprelude
import Hss.String.Types (OsStr, Path)
import Hss.String.Convert ()

import Control.Exception (try, bracket)
import Data.Word (Word32)
import Prelude (show) -- DELME
import System.IO.Error (ioError, isAlreadyExistsError)
import System.Random (randomIO)

import System.Directory.OsPath (withCurrentDirectory, removePathForcibly, createDirectory, getTemporaryDirectory)
import System.OsPath ((<.>), (</>), (-<.>), unsafeEncodeUtf, takeDirectory, takeBaseName)


-- TODO IntoOsStr over </> when ghc 9.12
-- (</>) :: (IntoOsStr a, IntoOsStr b) => a -> b -> Path
-- a </> b = toPath a Sys.</> toPath b

dirname :: Path -> Path
dirname = takeDirectory

basename :: Path -> OsStr
basename = takeBaseName

-- TODO re-export other path manipulation functions

-- FIXME withTempDir and withCd should go in an Environment module
--FIXME allow adding a template
withTempDir :: (Path -> IO a) -> IO a
withTempDir = bracket mkDir rmDir
  where
  mkDir :: IO Path
  mkDir = getTemporaryDirectory >>= loop
  loop :: Path -> IO Path
  loop parentDir = do
    nonce <- randomIO :: IO Word32
    let dirName = parentDir </> "hss" <.> (unsafeEncodeUtf . show) nonce
    try (createDirectory dirName) >>= \case
      Left exn | isAlreadyExistsError exn -> loop parentDir
               | True -> ioError exn
      Right () -> pure dirName
  -- rmDir = const $ pure () -- DEBUG
  rmDir = removePathForcibly

withCd :: Path -> IO a -> IO a
withCd = withCurrentDirectory
