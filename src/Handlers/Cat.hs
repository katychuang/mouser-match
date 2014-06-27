{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cat
  ( newCatHandler
  , createCatHandler
  , editCatHandler
  , updateCatHandler
  , destroyCatHandler
  , modifyCatHandler
  , showCatHandler
  ) where

import Control.Applicative
  ( (<|>)
  , (<$>)
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
  , heistLocal
  , renderWithSplices
  )
import Heist ((#!))
import Heist.Interpreted (textSplice)
import Application (App)
import Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  , AllCats(..)
  , UpdateCat(..)
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
import Text.Digestive.Heist (digestiveSplices)
import Control.Lens (view)
import Formlets.Cat.Create(createCatFormlet)
import Entities.Cat
  ( name
  , catData
  , catId
  , about
  , ownerName
  , temperament
  , Cat(..)
  )

modifyCatHandler :: Handler App App ()
modifyCatHandler = do
  hiddenMethod <- getParam "_method"
  case hiddenMethod of
    Just "put"    -> updateCatHandler
    Just "delete" -> destroyCatHandler
    _             -> notFound ""


newCatHandler :: Handler App App ()
newCatHandler = render "new_cat"

editCatHandler :: Handler App App ()
editCatHandler = do
  urlId <- reqParam "id"
  (v, _) <- runForm "form" createCatFormlet
  (Just cat) <- query (GetCat (read (unpack urlId)))
  let splices = do {
    "id"   #! textSplice (Data.Text.pack (show (view catId cat)));
    "name" #! textSplice (view (catData . name) cat);
    "ownerName" #! textSplice (view (catData . ownerName) cat);
    "temperament" #! textSplice (Data.Text.pack (show (fromEnum (view (catData . temperament) cat))));
    "about" #! textSplice (view (catData . about) cat);
  }
  renderWithSplices "edit_cat" (splices <> (digestiveSplices v))

showCatHandler :: Handler App App ()
showCatHandler = do
  urlId <- reqParam "id"
  (Just cat) <- query (GetCat (read (unpack urlId)))
  let splices = do {
    "id"   #! textSplice (Data.Text.pack (show (view catId cat)));
    "name" #! textSplice (view (catData . name) cat);
    "ownerName" #! textSplice (view (catData . ownerName) cat);
    "temperament" #! textSplice (Data.Text.pack (show (fromEnum (view (catData . temperament) cat))));
    "about" #! textSplice (view (catData . about) cat);
  }
  renderWithSplices "show_cat" splices

updateCatHandler :: Handler App App ()
updateCatHandler = do

  urlId <- read <$> unpack <$> reqParam "id"
  (_, result) <- runForm "form" createCatFormlet
  case result of
    Just catData -> do
      liftIO $ print catData
      update (UpdateCat (Cat {_catData = catData, _catId = urlId}))
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show urlId)))
    Nothing -> undefined



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

