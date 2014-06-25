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
  ( (<|>)
  )
import Data.ByteString.Char8
  ( pack
  , unpack
  )
import Data.Text
  ( pack
  )
import Snap.Core
  ( Method(..)
  , method
  , getParam
  , redirect
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
  , AllCats(..)
  )
import Control.Monad.IO.Class (liftIO)
import Snap.Extras.CoreUtils
  ( reqParam
  , notFound
  )
import Data.Monoid ((<>))
import Snap.Snaplet.AcidState
  ( query
  , update
  )
import Text.Digestive.Snap(runForm)
import Control.Lens (view)
import Formlets.Cat.Create(createCatFormlet)
import Entities.Cat
  ( name
  , catData
  , catId
  )

specificCatHandler :: Handler App App ()
specificCatHandler =
  method GET  showCatHandler <|> method POST changeCat 
  where
    changeCat = do
      hiddenMethod <- getParam "_method"
      case hiddenMethod of
        Just "put"    -> updateCatHandler
        Just "delete" -> destroyCatHandler
        _             -> notFound ""


newCatHandler :: Handler App App ()
newCatHandler = render "new_cat"

editCatHandler :: Handler App App ()
editCatHandler = render "edit_cat"

showCatHandler :: Handler App App ()
showCatHandler = do
  urlId <- reqParam "id"
  cs <- query AllCats
  liftIO $ putStrLn (show cs)
  (Just cat) <- query (GetCat (read (unpack urlId)))
  let splices = do {
    "id"   #! textSplice (Data.Text.pack (show (view catId cat)));
    "name" #! textSplice (view (catData . name) cat);
  }
  renderWithSplices "show_cat" splices

updateCatHandler :: Handler App App ()
updateCatHandler  = liftIO $ putStrLn "updating a cat!"

destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"

createCatHandler :: Handler App App ()
createCatHandler = method POST $ do
  (_, result) <- runForm "createCat" createCatFormlet
  case result of
    Just c -> do
      cr <- update (NewCat c)
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show (view catId cr))))
    Nothing -> undefined

