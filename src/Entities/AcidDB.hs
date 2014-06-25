{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Entities.AcidDB
  ( AcidDB(..)
  , newestCatId
  , cats
  ) where

import Control.Lens (makeLenses)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Data (Data, Typeable)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Monoid (Monoid(..))
import Data.IxSet
  ( Indexable(..)
  , ixGen
  , ixSet
  , Proxy(..)
  , IxSet
  )
import Entities.Cat (Cat)



data AcidDB = AcidDB
  { _newestCatId :: Int
  , _cats        :: IxSet Cat
  }
  deriving(Data, Typeable, Show, Eq)

$(deriveSafeCopy 0 'base ''AcidDB)


instance Monoid AcidDB where
  mempty = AcidDB
    { _newestCatId = 0
    , _cats = empty
    }
  mappend x y = 
    if (x == mempty)
      then y
      else x

makeLenses ''AcidDB
