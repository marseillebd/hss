module Hss.Preprelude
  ( -- * Booleans
    Bool(..)
  , (&&), (||), not
  , otherwise
  -- ** Relations
  , Eq(..)
  , Ord(..), Ordering
  -- * Integral Types
  , Int
  , Integer
  , Num(..), subtract, Integral(..), fromIntegral
  , Real(..)
  -- ** Enums
  , Enum(..), Bounded(..)
  -- * Strings
  -- ** Characters
  , Char, chr, ord
  -- * Floating Point
  , Float, Double
  , Fractional(..), Floating(..), RealFrac(..), RealFloat(..), realToFrac
  -- * Composites
  -- ** Structural Types
  , Maybe(..), maybe
  , Either(..)
  , fst, snd
  -- ** Containers
  , Semigroup(..), Monoid(..)
  -- * Functions
  , id, ($), seq, ($!), (.), (&)
  , const, flip, curry, uncurry
  -- ** Categories
  , Functor(..), (<$>), (<&>)
  , Bifunctor(..)
  , Applicative(..)
  , Alternative(..)
  , Monad(..), (=<<), forM_
  -- * IO
  , IO, MonadIO(..)
  -- ** System
  , ExitCode(..), exitSuccess, exitFailure, exitWith
  -- * Bottom
  , undefined
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Char (chr, ord)
import Data.Function
import Data.Functor
import Prelude hiding (undefined)
import System.Exit (ExitCode(..))

import qualified Prelude
import qualified System.Exit as Sys

exitSuccess :: MonadIO io => io a
exitSuccess = liftIO Sys.exitSuccess

exitFailure :: MonadIO io => io a
exitFailure = liftIO Sys.exitFailure

exitWith :: MonadIO io => ExitCode -> io a
exitWith = liftIO . Sys.exitWith

undefined :: a
undefined = Prelude.undefined
{-# WARNING undefined "undefined is for temp use only; implement missing functionality, report errors, or panic" #-}
