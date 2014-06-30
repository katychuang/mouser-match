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
import Data.Text.Encoding (decodeUtf8)
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
import Heist ((#!), splicesToList)
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
import Text.Digestive.Heist (bindDigestiveSplices, digestiveSplices)
import Control.Lens (view)
import Formlets.Cat
  ( catFormlet
  , catDataFormlet
  )
import Entities.Cat
  ( name
  , catData
  , catId
  , about
  , ownerName
  , temperament
  , location
  , Cat(..)
  )

import Data.Text(Text)
import Heist.Interpreted(Splice)
import Text.Digestive.View(viewInput, getForm, postForm)
import Choice (Choice(..))

modifyCatHandler :: Handler App App ()
modifyCatHandler = do
  hiddenMethod <- getParam "_method"
  case hiddenMethod of
    Just "put"    -> updateCatHandler
    Just "delete" -> destroyCatHandler
    _             -> notFound ""


newCatHandler :: Handler App App ()
newCatHandler = do 
  (dfView, _) <- runForm "" (catDataFormlet Nothing)
  heistLocal (bindDigestiveSplices  dfView) $ render "new_cat"

editCatHandler :: Handler App App ()
editCatHandler = do
  urlId <- reqParam "id"
  cat <- query (GetCat (read (unpack urlId)))
  (dfView, result) <- runForm "" (catFormlet cat)
  let splices = "id" #! textSplice (decodeUtf8 urlId)
  renderWithSplices "edit_cat" (digestiveSplices dfView <> splices)

showCatHandler :: Handler App App ()
showCatHandler = do
  urlId <- reqParam "id"
  (Just cat) <- query (GetCat (read (unpack urlId)))
  let splices = do {
    "id"   #! textSplice (Data.Text.pack (show (view catId cat)));
    "name" #! textSplice (view (catData . name) cat);
    "ownerName" #! textSplice (view (catData . ownerName) cat);
    "location" #! textSplice (renderChoice (view (catData . location) cat));
    "temperament" #! textSplice (renderChoice (view (catData . temperament) cat));
    "about" #! textSplice (view (catData . about) cat);
  };
  renderWithSplices "show_cat" splices

updateCatHandler :: Handler App App ()
updateCatHandler = do
  (v', result) <- runForm "" (catFormlet Nothing)
  case result of
    Just cat -> do
      update (UpdateCat cat)
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show (view catId cat))))
    Nothing -> redirect "/cat/create"



destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"

createCatHandler :: Handler App App ()
createCatHandler = method POST $ do
  (_, result) <- runForm "" (catDataFormlet Nothing)
  case result of
    Just c -> do
      cr <- update (NewCat c)
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show (view catId cr))))
    Nothing -> undefined

