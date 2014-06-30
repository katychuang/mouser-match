{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  , AllCats(..)
  , UpdateCat(..)
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
  , (^.)
  , (+=)
  , (%=)
  , use
  )
import Entities.Cat
  ( Cat(..)
  , CatData(..)
  , catId
  )
import Entities.AcidDB
  ( AcidDB(..)
  , cats
  , newestCatId
  )
import Control.Applicative ((<$>))

newCat :: CatData -> Update AcidDB Cat
newCat c = do
    id <- use newestCatId
    let new = Cat {_catId = id, _catData = c}
    cats %= (\cs -> insert new cs)
    newestCatId += 1
    return new
  
getCat :: Int -> Query AcidDB (Maybe Cat)
getCat catId = do
  cs <- view cats
  return $ getOne (cs @= catId)

updateCat :: Cat -> Update AcidDB ()
updateCat cat = cats %= updateIx (cat^.catId) cat
      
allCats :: Query AcidDB [Cat]
allCats = toList <$> view cats

$(makeAcidic ''AcidDB ['newCat, 'getCat, 'allCats, 'updateCat])

