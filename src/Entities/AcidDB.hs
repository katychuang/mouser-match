{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Entities.AcidDB
  ( AcidDB(..)
  , newestCatId
  , cats
  ) where

import           Control.Lens    (makeLenses)
import           Data.ByteString (ByteString)
import           Data.Data       (Data, Typeable)
import           Data.IxSet      (Indexable (..), IxSet, Proxy (..), ixGen,
                                  ixSet)
import           Data.Monoid     (Monoid (..))
import           Data.SafeCopy   (base, deriveSafeCopy)
import           Data.Text       (Text)
import           Entities.Cat    (Cat)

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
