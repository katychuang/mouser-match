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

import           Application            (App)
import           Choice                 (Choice (..))
import           Control.Applicative    ((<$>), (<|>))
import           Control.Arrow          ((>>>))
import           Control.Lens           (to, (^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Monoid            ((<>))
import           Data.Text              (pack)
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           Entities.Cat           (Cat (..), about, catData, catId,
                                         location, name, ownerName, temperament)
import           Formlets.Cat           (catDataFormlet, catFormlet)
import           Heist                  (splicesToList, ( #! ))
import           Heist.Interpreted      (textSplice)
import           Heist.Interpreted      (Splice)
import           Queries.Cat            (AllCats (..), GetCat (..), NewCat (..),
                                         UpdateCat (..))
import           Snap.Core              (Method (..), getParam, method,
                                         redirect)
import           Snap.Extras.CoreUtils  (notFound, reqParam)
import           Snap.Snaplet           (Handler)
import           Snap.Snaplet.AcidState (query, update)
import           Snap.Snaplet.Heist     (heistLocal, render, renderWithSplices)
import           Text.Digestive.Heist   (bindDigestiveSplices, digestiveSplices)
import           Text.Digestive.Snap    (runForm)
import           Text.Digestive.View    (getForm, postForm, viewInput)

modifyCatHandler :: Handler App App ()
modifyCatHandler = do
  hiddenMethod <- getParam "_method"
  case hiddenMethod of
    Just "put"    -> updateCatHandler
    Just "delete" -> destroyCatHandler
    _             -> notFound ""

newCatHandler :: Handler App App ()
newCatHandler = do
  (dfView, _) <- runForm "" $ catDataFormlet Nothing
  heistLocal (bindDigestiveSplices  dfView) $ render "new_cat"

editCatHandler :: Handler App App ()
editCatHandler = do
  urlId <- reqParam "id"
  cat   <- query $ GetCat $ urlId^.to (unpack >>> read)
  (view, _) <- runForm "" $ catFormlet cat

  let splices = "id" #! urlId^.to (decodeUtf8 >>> textSplice)
  renderWithSplices "edit_cat" $ digestiveSplices view <> splices

showCatHandler :: Handler App App ()
showCatHandler = do
  urlId <- reqParam "id"
  result <- query (GetCat (read (unpack urlId)))
  case result of
    Just cat -> do
      let catText   lens = cat^.catData.lens.to textSplice
      let catChoice lens = cat^.catData.lens.to (renderChoice >>> textSplice)

      let splices = do {
        "id"          #! cat^.catId.to (show >>> Data.Text.pack >>> textSplice);
        "name"        #! catText name;
        "ownerName"   #! catText ownerName;
        "location"    #! catChoice location;
        "temperament" #! catChoice temperament;
        "about"       #! catText about;
      };
      renderWithSplices "show_cat" splices
    Nothing -> error "bad show"

updateCatHandler :: Handler App App ()
updateCatHandler = do
  (_, result) <- runForm "" (catFormlet Nothing)
  case result of
    Just cat -> do
      update $ UpdateCat cat
      redirect $ "/cat/" <> cat^.catId.to (show >>> Data.ByteString.Char8.pack)
    Nothing -> redirect "/cat/create"

destroyCatHandler :: Handler App App ()
destroyCatHandler = liftIO $ putStrLn "destroying a cat!"

createCatHandler :: Handler App App ()
createCatHandler = do
  (_, result) <- runForm "" (catDataFormlet Nothing)
  case result of
    Just catData -> do
      cat <- update (NewCat catData)
      redirect $ "/cat/" <> cat^.catId.to (show >>> Data.ByteString.Char8.pack)
    Nothing -> undefined
