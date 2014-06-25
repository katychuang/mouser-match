{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Entities.Cat
  ( Cat(..)
  , Temperament(..)
  , Base64Picture(..)
  , catId
  , name
  , ownerName
  , temperament
  , about
  , picture
  ) where

import Control.Lens (makeLenses)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Data (Data, Typeable)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Monoid (Monoid(..))

data Temperament = Friendly 
                 | Shy
                 | Fiery
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Temperament)

newtype Base64Picture = Base64Picture ByteString
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Base64Picture)

data Cat = Cat
  { _catId       :: Maybe Int
  , _name        :: Text
  , _ownerName   :: Text
  , _temperament :: Temperament
  , _about       :: Text
  , _picture     :: Base64Picture
  }
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Cat)

instance Monoid Cat where
  mempty = Cat Nothing "" "" Friendly "" (Base64Picture "")
  mappend x y = 
    if (x == mempty)
      then y
      else x

makeLenses ''Cat
