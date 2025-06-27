module Hss.Process
  ( Shell, runShell
  , ExtCmd(..), ExtProc(..), spawnExt
  , simple
  , ExitCode(..), exitSuccess, exitFailure
  ) where

import Hss.Preprelude
import Hss.String

import qualified Hss.String.Bytes as B

import Data.Map (Map)

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Hss.String.Convert (strOs2Hs)
import System.Directory.OsPath (getCurrentDirectory)
import System.IO (Handle)
import System.Process (CreateProcess(..), createProcess_, ProcessHandle, createPipe)
import System.Process.Environment.OsString (getEnvironment)
import Data.Kind (Type)

import qualified System.IO as Sys
import qualified System.Process as Process
import qualified Data.Map as Map

-- TODO what do I actually want to do?
-- cmd1 | cmd2
-- cmd m>&n, cmd n>file cmd m>>file
-- cmd <file, cmd2 <$(cmd2)
-- hsvar=$(cmd)
-- envvar=foo cmd
-- export envvar=foo
-- cmd &; pid=$!

---------------------------------
------------ Process ------------
---------------------------------

data ExtCmd = ExtCmd
  { prog :: OsStr
  , args :: [OsStr]
  }

data ExtProc = ExtProc
  { cmd :: ExtCmd
  , proc :: ProcessHandle
  }

spawnExt :: ExtCmd -> Shell ExtProc
spawnExt cmd = Sh $ \env -> do
  (_, _, _, p) <- createProcess_ "Hss.Process.spawnExt" CreateProcess
    { cmdspec = Process.RawCommand (strOs2Hs cmd.prog) (strOs2Hs <$> cmd.args)
    , cwd = Just $ strOs2Hs env.st_pwd
    , env = Just $ bimap strOs2Hs strOs2Hs <$> Map.toList env.st_env
    , std_in = Process.UseHandle env.st_handles.stdin
    , std_out = Process.UseHandle env.st_handles.stdout
    , std_err = Process.UseHandle env.st_handles.stderr
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
    }
  liftIO (Process.waitForProcess proc.proc) >>= \case
    ExitSuccess -> pure ()
    ExitFailure _ -> undefined

-------------------------------------
------------ Shell Monad ------------
-------------------------------------

newtype Shell a = Sh { unSh :: State -> IO a }

data State = ShSt
  { st_pwd :: !Path
  , st_env :: !(Map OsStr OsStr)
  , st_handles :: !Handles
  }

data Handles = Handles
  { stdin :: Handle
  , stdout :: Handle
  , stderr :: Handle
  }

stdHandles :: Handles
stdHandles = Handles
  { stdin = Sys.stdin
  , stdout = Sys.stdout
  , stderr = Sys.stderr
  }

runShell :: Shell a -> IO a
runShell sh = do
  st_pwd <- getCurrentDirectory
  st_env <- Map.fromList <$> getEnvironment
  let st_handles = stdHandles
  unSh sh ShSt{st_pwd,st_env,st_handles}

instance Functor Shell where
  fmap f x = Sh $ \envRef -> f <$> unSh x envRef

instance Applicative Shell where
  pure x = Sh $ \_ -> pure x
  f <*> x = Sh $ \envRef -> unSh f envRef <*> unSh x envRef

instance Monad Shell where
  getX >>= k = Sh $ \envRef -> unSh getX envRef >>= \x -> unSh (k x) envRef

instance MonadIO Shell where
  liftIO action = Sh $ \_ -> action
