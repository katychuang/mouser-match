{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  ) where

import           Application         (App)
import           Data.ByteString     (ByteString)
import           Handlers.Cat        (createCatHandler, destroyCatHandler,
                                      editCatHandler, modifyCatHandler,
                                      newCatHandler, showCatHandler,
                                      updateCatHandler)
import           Snap.Core           (Method (..), method)
import           Snap.Snaplet        (Handler)
import           Snap.Util.FileServe (serveDirectory)

routes :: [(ByteString, Handler App App ())]
routes = [ ("/cat/new",      method GET    newCatHandler)
         , ("/cat/:id/edit", method GET    editCatHandler)
         , ("/cat/:id",      method GET    showCatHandler)
         , ("/cat/:id",      method POST   modifyCatHandler)
         , ("/cat/:id",      method PUT    updateCatHandler)
         , ("/cat/:id",      method DELETE destroyCatHandler)
         , ("/cat",          method POST   createCatHandler)
         , ("",              serveDirectory "static")
         ]
