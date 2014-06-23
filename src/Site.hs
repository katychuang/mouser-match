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
import qualified Text.Digestive.Snap                         as DFS
import           Text.Digestive.View
import Validations
import Control.Arrow ((>>>))
import Control.Lens
import qualified Snap.Snaplet.AcidState as AS


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

specificCatHandler :: Handler App App ()
specificCatHandler =
  method GET  showCatHandler <|> method POST changeCat 
  where
    changeCat = do
      method <- getParam "_method"
      case method of
        Just "put"    -> updateCatHandler
        Just "delete" -> destroyCatHandler
        _             -> notFound ""


newCatHandler :: Handler App App ()
newCatHandler     = render "new_cat"

editCatHandler :: Handler App App ()
editCatHandler    = render "edit_cat"

showCatHandler :: Handler App App ()
showCatHandler = do
  id <- reqParam "id"
  cat <- query GetCat
  let splices = do {
    "id" ## I.textSplice (decodeUtf8 id);
    "name" ## I.textSplice (view name cat);
  }
  renderWithSplices "show_cat" splices

updateCatHandler :: Handler App App ()
updateCatHandler  = liftIO $ putStrLn "updating a cat!"

destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"


createCatForm :: (Monad m) => Form T.Text m Cat 
createCatForm = Cat
  <$> "name"        .: text Nothing
  <*> "ownerName"   .: text Nothing
  <*> "temperament" .: choice
                       [ (Friendly, "friendly")
                       , (Shy,      "shy")
                       , (Fiery,    "fiery")
                       ] Nothing
  <*> "about"       .: text Nothing
  <*> (pure (Base64Picture "hi"))


nonEmpty :: (Monoid a, Eq a) => Checker T.Text a a
nonEmpty x =
  if (x == mempty)
    then Left "is empty"
    else Right x

{-
createCatValidation :: (Monad m) => Cat -> EitherT m [(T.Text, T.Text)] Cat
createCatValidation cat = do
  validation (view name cat) $ do
    nonEmpty `attach` "name"

  validation (view ownerName cat) $ do
    nonEmpty `attach` "name"

  validation name (view ownerName cat) (
    nonEmpty `attach` "ownerName"
  )

-}


createCatHandler :: Handler App App ()
createCatHandler = method POST $ do
  (view, result) <- DFS.runForm "createCat" createCatForm
  case result of
    Just c -> do
      AS.update (NewCat c)
      return ()
    Nothing -> undefined


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        with auth handleLoginSubmit)
         , ("/logout",       with auth handleLogout)
         , ("/new_user",     with auth handleNewUser)
         , ("/cat/new",      newCatHandler)
         , ("/cat/:id/edit", editCatHandler)
         , ("/cat/:id",      specificCatHandler)
         , ("/cat",          createCatHandler)
         , ("",              serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
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

