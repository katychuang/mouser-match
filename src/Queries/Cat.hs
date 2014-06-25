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

newCat :: CatData -> Update Cat Cat
newCat c = put new >> return new 
  where new = Cat {_catId = 0, _catData = c}
  
getCat :: Query Cat Cat
getCat = ask

$(makeAcidic ''Cat ['newCat, 'getCat])

