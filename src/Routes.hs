{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  ) where

import Data.ByteString (ByteString)
import Snap.Snaplet (Handler)
import Snap.Util.FileServe (serveDirectory)
import Application (App)
import Snap.Core
  ( Method(..)
  , method
  )
import Handlers.Cat
  ( newCatHandler
  , editCatHandler
  , modifyCatHandler
  , createCatHandler
  , updateCatHandler
  , destroyCatHandler
  , showCatHandler
  )

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
