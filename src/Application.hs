{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Application where

import           Control.Lens           (makeLenses, view)
import           Entities.AcidDB        (AcidDB)
import           Snap.Snaplet           (Handler, Snaplet, snapletValue,
                                         subSnaplet)
import           Snap.Snaplet.AcidState (Acid, HasAcid, getAcidStore)
import           Snap.Snaplet.Auth      (AuthManager)
import           Snap.Snaplet.Heist     (HasHeist (..), Heist)
import           Snap.Snaplet.Session   (SessionManager)

data App = App
    { _heist     :: Snaplet (Heist App)
    , _sess      :: Snaplet SessionManager
    , _auth      :: Snaplet (AuthManager App)
    , _acidState :: Snaplet (Acid AcidDB)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App

instance HasAcid App AcidDB where
  getAcidStore = view (acidState . snapletValue)
