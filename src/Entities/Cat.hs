{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Entities.Cat
  ( Cat(..)
  , CatData(..)
  , Temperament(..)
  , Base64Picture(..)
  , catId
  , name
  , ownerName
  , temperament
  , about
  , picture
  , catData
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
  )

data Temperament = Friendly 
                 | Shy
                 | Fiery
  deriving(Data, Typeable, Show, Eq, Ord, Enum)
$(deriveSafeCopy 0 'base ''Temperament)

newtype Base64Picture = Base64Picture ByteString
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Base64Picture)

data CatData = CatData
  { _name        :: Text
  , _ownerName   :: Text
  , _temperament :: Temperament
  , _about       :: Text
  , _picture     :: Base64Picture
  }
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''CatData)


data Cat = Cat
  { _catId       :: Int
  , _catData     :: CatData
  }
  deriving(Data, Typeable, Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Cat)


instance Monoid CatData where
  mempty = CatData "" "" Friendly "" (Base64Picture "")
  mappend x y = 
    if (x == mempty)
      then y
      else x

instance Indexable Cat where
  empty = ixSet [ ixGen (Proxy :: Proxy Int) ]

makeLenses ''CatData
makeLenses ''Cat
