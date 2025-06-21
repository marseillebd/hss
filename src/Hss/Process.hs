module Hss.Process
  ( Shell
  , pwd
  ) where

import Preprelude

import Hss.Path (OsString, OsPath)
import Data.String (fromString)
import Data.Map (Map)

import System.Process.Environment.OsString (getEnvironment)
import System.Directory.OsPath (getCurrentDirectory)
import System.IO (Handle)
import System.Process (CreateProcess(..), createProcess_, ProcessHandle)

import qualified System.Process as Process
import qualified Data.Map as Map

---------------------------------
------------ Process ------------
---------------------------------

data ExtCmd = ExtCmd
  { prog :: OsPath
  , args :: [OsPath]
  , cwd :: OsPath
  , stdin :: Handle
  , stdout :: Handle
  , stderr :: Handle
  }

data ExtProc = ExtProc
  { prog :: ExtCmd
  , procHandle :: ProcessHandle
  , stdin :: Handle
  , stdout :: Handle
  , stderr :: Handle
  }

spawnExt :: ExtCmd -> Shell ExtProc
spawnExt cmd = Sh $ \env -> do
  (i, o, e, p) <- createProcess_ "Hss.Process.spawnExt" CreateProcess
    { cmdspec = Process.RawCommand (fromString cmd.prog) (fromString <$> cmd.args)
    , cwd = Nothing -- TODO
    , env = Nothing -- TODO
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
  undefined

-------------------------------------
------------ Shell Monad ------------
-------------------------------------

newtype Shell a = Sh { unSh :: State -> IO a }

data State = ShSt
  { st_pwd :: !OsPath
  , st_env:: !(Map OsString OsString)
  }

runShell :: Shell a -> IO a
runShell sh = do
  st_pwd <- getCurrentDirectory
  st_env <- Map.fromList <$> getEnvironment
  unSh sh ShSt{st_pwd,st_env}

pwd :: Shell OsPath
pwd = Sh $ pure . st_pwd

cd :: OsPath -> Shell ()
cd = undefined

instance Functor Shell where
  fmap f x = Sh $ \env -> f <$> unSh x env

instance Applicative Shell where
  pure x = Sh $ \_ -> pure x
  f <*> x = Sh $ \env -> unSh f env <*> unSh x env

instance Monad Shell where
  getX >>= k = Sh $ \env -> unSh getX env >>= \x -> unSh (k x) env

instance MonadIO Shell where
  liftIO action = Sh $ \_ -> action
