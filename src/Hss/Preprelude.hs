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
  -- * Bottom
  , undefined
  ) where

import Prelude hiding (undefined)
import Control.Applicative
import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Data.Bifunctor
import Data.Char (chr, ord)
import Control.Monad

import qualified Prelude

undefined :: a
undefined = Prelude.undefined
{-# WARNING undefined "undefined is for temp use only; implement missing functionality, report errors, or panic" #-}
