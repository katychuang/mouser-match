{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cat
  ( newCatHandler
  , createCatHandler
  , editCatHandler
  , updateCatHandler
  , destroyCatHandler
  , specificCatHandler
  ) where

import Control.Applicative
  ( (<$>)
  , (<*>)
  , (<|>)
  )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Snap.Core
  ( Method(..)
  , method
  , getParam
  )
import Snap.Snaplet (Handler)
import Snap.Snaplet.Heist
  ( render
  , renderWithSplices
  )
import Heist ((#!))
import Heist.Interpreted (textSplice)
import Application (App)
import Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  )
import Control.Monad.IO.Class (liftIO)
import Snap.Extras.CoreUtils
  ( reqParam
  , notFound
  )
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid ((<>))
import Snap.Snaplet.AcidState
  ( query
  , update
  )
import Text.Digestive.Snap(runForm)
import Control.Lens (view)
import Formlets.Cat.Create(createCatFormlet)
import Entities.Cat
  ( Cat
  , name
  )

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
    "id"   #! textSplice (decodeUtf8 id);
    "name" #! textSplice (view name cat);
  }
  renderWithSplices "show_cat" splices

updateCatHandler :: Handler App App ()
updateCatHandler  = liftIO $ putStrLn "updating a cat!"

destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"

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
  (view, result) <- runForm "createCat" createCatFormlet
  case result of
    Just c -> do
      update (NewCat c)
      return ()
    Nothing -> undefined

