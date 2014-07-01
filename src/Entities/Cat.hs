{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Entities.Cat
  ( Cat(..)
  , CatData(..)
  , Location(..)
  , Temperament(..)
  , Base64Picture(..)
  , catId
  , name
  , ownerName
  , temperament
  , location
  , about
  , picture
  , catData
  ) where

import           Control.Lens    (makeLenses)
import           Data.ByteString (ByteString)
import           Data.Data       (Data, Typeable)
import           Data.IxSet      (Indexable (..), Proxy (..), ixGen, ixSet)
import           Data.Monoid     (Monoid (..))
import           Data.SafeCopy   (base, deriveSafeCopy)
import           Data.Text       (Text)

data Temperament = Friendly
                 | Shy
                 | Fiery
  deriving(Data, Typeable, Show, Eq, Ord, Enum)
$(deriveSafeCopy 0 'base ''Temperament)

data Location = Soho
              | Tribeca
              | Chelsea
              | LowerEastSide
              | FortGreene
              | ProspectHeights
              | Williamsburg
              | Bushwick
              | CrownHeights
  deriving(Data, Typeable, Show, Eq, Ord, Enum)
$(deriveSafeCopy 0 'base ''Location)

newtype Base64Picture = Base64Picture ByteString
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Base64Picture)

data CatData = CatData
  { _name        :: Text
  , _ownerName   :: Text
  , _location    :: Location
  , _temperament :: Temperament
  , _about       :: Text
  , _picture     :: Base64Picture
  }
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''CatData)


data Cat = Cat
  { _catId   :: Int
  , _catData :: CatData
  }
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Cat)


instance Indexable Cat where
  empty = ixSet [ ixGen (Proxy :: Proxy Int) ]

makeLenses ''CatData
makeLenses ''Cat
