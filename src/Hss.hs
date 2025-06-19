module Hss
  ( BS.ByteString
  -- * Shell
  , exe
  , (|>)
  , (&>)
  , (&>>)
  , capture
  -- * Environment
  , getArgs
  , withCd
  , withTempDir
  -- * Strings
  , fromString
  , here
  , StringLike(..)
  , convertStringLike
  , MoreStringLike(..)
  , lines
  -- * Paths
  , (</>)
  , (<.>)
  , dirname
  , basename
  ) where

import Prelude hiding (break, lines)
import qualified Prelude

import Shh (exe, (|>), capture)
import Data.String (fromString)
import Data.String.Here.Uninterpolated (here)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import Data.Word (Word32)
import System.Random (randomIO)
import Control.Exception (try, bracket)
import System.IO.Error (isAlreadyExistsError)
import Data.String.Like (StringLike(..), convertStringLike)

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Shh
import qualified Data.ByteString.Lazy.Char8 as LBS

infixl 9 &>
(&>) :: Shh.Shell m => Shh.Proc a -> FilePath -> m a
p &> fpath = p Shh.&> Shh.Truncate (fromString fpath)

infixl 9 &>>
(&>>) :: Shh.Shell m => Shh.Proc a -> FilePath -> m a
p &>> fpath = p Shh.&> Shh.Append (fromString fpath)

withCd :: FilePath -> IO a -> IO a
withCd = Dir.withCurrentDirectory

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = bracket mkDir rmDir
  where
  mkDir :: IO FilePath
  mkDir = Dir.getTemporaryDirectory >>= loop
  loop :: FilePath -> IO FilePath
  loop parentDir = do
    nonce <- randomIO :: IO Word32
    let dirName = parentDir </> "hss" <.> show nonce
    try (Dir.createDirectory dirName) >>= \case
      Left exn | isAlreadyExistsError exn -> loop parentDir
               | otherwise -> ioError exn
      Right () -> pure dirName
  rmDir = const $ pure () -- DEBUG
  -- rmDir = Dir.removePathForcibly

dirname :: FilePath -> FilePath
dirname = Path.takeDirectory

basename :: FilePath -> String
basename = Path.takeBaseName

class StringLike a => MoreStringLike a where
  -- TODO span
  break :: (Char -> Bool) -> a -> (a, a)

instance MoreStringLike String where
  break = Prelude.break
instance MoreStringLike LBS.ByteString where
  break = LBS.break

lines :: MoreStringLike str => str -> [str]
lines str | strNull str = []
lines str =
  let (before, rest) = break (== '\n') str
   in before : maybe [] (lines . snd) (uncons rest)
