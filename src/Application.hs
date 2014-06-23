{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState(Acid, HasAcid, getAcidStore)
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.ByteString
import Control.Applicative
import Data.ByteString
import Data.Text

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App

data Temperament = Friendly 
                 | Shy
                 | Fiery
  deriving(Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Temperament)

newtype Base64Picture = Base64Picture ByteString
  deriving(Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Base64Picture)

data Cat = Cat
  { _name        :: Text
  , _ownerName   :: Text
  , _temperament :: Temperament
  , _about       :: Text
  , _picture     :: Base64Picture
  }

  
