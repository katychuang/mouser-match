{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Snap.Snaplet
  ( makeSnaplet
  , nestSnaplet
  , addRoutes
  , SnapletInit
  )
import Snap.Snaplet.Auth
  ( defAuthSettings
  , addAuthSplices
  )
import Snap.Snaplet.Auth.Backends.JsonFile (initJsonFileAuthManager)
import Snap.Snaplet.Heist (heistInit)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Application
  ( App(..)
  , heist
  , auth
  , acidState
  , sess
  )
import Snap.Snaplet.AcidState (acidInit)
import Routes (routes)
import Data.Monoid (mempty)

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    aS <- nestSnaplet "" acidState $ acidInit mempty
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a aS

