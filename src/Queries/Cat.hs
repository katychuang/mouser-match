{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  , AllCats(..)
  ) where

import Data.Acid
  ( makeAcidic
  , Update
  , Query
  )
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class
  ( put
  , get
  )
import Data.IxSet
import Control.Lens
  ( view
  , over
  )
import Entities.Cat
  ( Cat(..)
  , CatData(..)
  )
import Entities.AcidDB
  ( AcidDB(..)
  , cats
  , newestCatId
  )

newCat :: CatData -> Update AcidDB Cat
newCat c = do
    acidDB <- get
    let newId = (view newestCatId acidDB) + 1
    let new = Cat {_catId = newId, _catData = c}
    let updatedIx = over cats (\cs -> insert new cs) acidDB
    let updatedId = over newestCatId (\i -> i + 1) updatedIx
    put updatedId
    return new
  
getCat :: Int -> Query AcidDB (Maybe Cat)
getCat catId = do 
  acidDB <- ask
  return $ getOne ((view cats acidDB) @= catId)


allCats :: Query AcidDB [Cat]
allCats = ask >>= \adb -> return $ toList (view cats adb)

$(makeAcidic ''AcidDB ['newCat, 'getCat, 'allCats])

