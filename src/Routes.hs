{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  ) where

import Data.ByteString (ByteString)
import Snap.Snaplet (Handler)
import Snap.Util.FileServe (serveDirectory)
import Application (App)
import Handlers.Cat
  ( newCatHandler
  , editCatHandler
  , specificCatHandler
  , createCatHandler
  )

routes :: [(ByteString, Handler App App ())]
routes = [ ("/cat/new",      newCatHandler)
         , ("/cat/:id/edit", editCatHandler)
         , ("/cat/:id",      specificCatHandler)
         , ("/cat",          createCatHandler)
         , ("",              serveDirectory "static")
         ]
