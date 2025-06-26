module Hss.Process
  ( Shell, runShell
  , pwd, cd
  , ExtCmd(..), ExtProc(..), spawnExt
  , simple
  ) where

import Hss.Preprelude

import Hss.String (OsStr, Path)
import Data.Map (Map)

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Hss.String.Convert (strOs2Hs)
import System.Directory.OsPath (getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import System.Process (CreateProcess(..), createProcess_, ProcessHandle)
import System.Process.Environment.OsString (getEnvironment)

import qualified System.IO as Sys
import qualified System.Process as Process
import qualified Data.Map as Map

---------------------------------
------------ Process ------------
---------------------------------

data ExtCmd = ExtCmd
  { prog :: OsStr
  , args :: [OsStr]
  , stdin :: Handle
  , stdout :: Handle
  , stderr :: Handle
  }

data ExtProc = ExtProc
  { cmd :: ExtCmd
  , proc :: ProcessHandle
  }

spawnExt :: ExtCmd -> Shell ExtProc
spawnExt cmd = Sh $ \envRef -> do
  env <- readIORef envRef
  (_, _, _, p) <- createProcess_ "Hss.Process.spawnExt" CreateProcess
    { cmdspec = Process.RawCommand (strOs2Hs cmd.prog) (strOs2Hs <$> cmd.args)
    , cwd = Just $ strOs2Hs env.st_pwd
    , env = Just $ bimap strOs2Hs strOs2Hs <$> Map.toList env.st_env
    , std_in = Process.UseHandle cmd.stdin
    , std_out = Process.UseHandle cmd.stdout
    , std_err = Process.UseHandle cmd.stderr
    , close_fds = False
    , create_group = False
    , delegate_ctlc = False
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    }
  pure ExtProc
    { cmd = cmd
    , proc = p
    }

simple :: OsStr -> [OsStr] -> Shell ()
simple cmd args = do
  proc <- spawnExt ExtCmd
    { prog = cmd
    , args
    , stdin = Sys.stdin
    , stdout = Sys.stdout
    , stderr = Sys.stderr
    }
  liftIO (Process.waitForProcess proc.proc) >>= \case
    ExitSuccess -> pure ()
    ExitFailure _ -> undefined

-------------------------------------
------------ Shell Monad ------------
-------------------------------------

newtype Shell a = Sh { unSh :: IORef State -> IO a }

data State = ShSt
  { st_pwd :: !Path
  , st_env :: !(Map OsStr OsStr)
  }

runShell :: Shell a -> IO a
runShell sh = do
  st_pwd <- getCurrentDirectory
  st_env <- Map.fromList <$> getEnvironment
  envRef <- newIORef $ ShSt{st_pwd,st_env}
  unSh sh envRef

pwd :: Shell Path
pwd = Sh $ \envRef -> do
  env <- readIORef envRef
  pure env.st_pwd

cd :: Path -> Shell ()
cd newDir = Sh $ \envRef ->
  modifyIORef envRef $ \env -> env{st_pwd = newDir}

instance Functor Shell where
  fmap f x = Sh $ \envRef -> f <$> unSh x envRef

instance Applicative Shell where
  pure x = Sh $ \_ -> pure x
  f <*> x = Sh $ \envRef -> unSh f envRef <*> unSh x envRef

instance Monad Shell where
  getX >>= k = Sh $ \envRef -> unSh getX envRef >>= \x -> unSh (k x) envRef

instance MonadIO Shell where
  liftIO action = Sh $ \_ -> action
