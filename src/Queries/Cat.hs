{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  ) where

import Data.Acid
  ( makeAcidic
  , Update
  , Query
  )
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (put)
import Data.IxSet
import Entities.Cat

newCat :: Cat -> Update Cat ()
newCat c = put c
  
getCat :: Query Cat Cat
getCat = ask

$(makeAcidic ''Cat ['newCat, 'getCat])

