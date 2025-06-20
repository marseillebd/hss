module Hss.IO
  ( IO
  , MonadIO(..)
  , IOMode(..)
  -- TODO more exports
  -- TODO PrimMonad?
  ) where

import Control.Monad.IO.Class
import System.IO
