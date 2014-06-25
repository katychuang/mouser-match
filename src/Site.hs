{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Control.Monad.IO.Class
import           Snap.Extras.CoreUtils
import           Data.Text.Encoding
import           Data.Monoid
import Snap.Snaplet.AcidState
import           Text.Digestive
import           Text.Digestive.Heist
import Text.Digestive.Snap(runForm)
import           Text.Digestive.View
import Control.Arrow ((>>>))
import Control.Lens
import qualified Snap.Snaplet.AcidState as AS
import Formlets.Cat.Create(createCatFormlet)
import Entities.Cat
import Handlers.Cat
import Routes (routes)

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

