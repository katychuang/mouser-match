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
import Forms.Cat
  ( catForm
  , catDataForm
  )
import Entities.Cat
  ( name
  , catData
  , catId
  , about
  , ownerName
  , temperament
  , Cat(..)
  )

import Data.Text(Text)
import Heist.Interpreted(Splice)

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
  cat <- query (GetCat (read (unpack urlId)))
  (v, result) <- runForm "form" (catForm cat)
  let splices = do {
    "id" #! textSplice (decodeUtf8 urlId);
  }
  renderWithSplices "edit_cat" (digestiveSplices v <> splices)

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
  let urlId = 2
--  urlId <- read <$> unpack <$> reqParam "id"
  (_, result) <- runForm "form" (catForm Nothing)
  liftIO $ print result
  case result of
    Just cat -> do
      liftIO $ print cat
      update (UpdateCat cat)
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show urlId)))
    Nothing -> undefined



destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"

createCatHandler :: Handler App App ()
createCatHandler = method POST $ do
  (_, result) <- runForm "createCat" (catDataForm Nothing)
  case result of
    Just c -> do
      cr <- update (NewCat c)
      redirect ("/cat/" <> (Data.ByteString.Char8.pack (show (view catId cr))))
    Nothing -> undefined

