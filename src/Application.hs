{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Application where

import Control.Lens (makeLenses, view)
import Snap.Snaplet
  ( Snaplet
  , snapletValue
  , Handler
  , subSnaplet
  )
import Snap.Snaplet.Heist 
  ( Heist
  , HasHeist(..)
  )
import Snap.Snaplet.Auth (AuthManager)
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.AcidState
  ( Acid
  , HasAcid
  , getAcidStore
  )
import Entities.Cat (Cat)

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _acidState :: Snaplet (Acid Cat)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App

instance HasAcid App Cat where
  getAcidStore = view (acidState . snapletValue)
