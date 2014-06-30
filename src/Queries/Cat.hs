{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Queries.Cat
  ( NewCat(..)
  , GetCat(..)
  , AllCats(..)
  , UpdateCat(..)
  ) where

import           Control.Applicative        ((<$>))
import           Control.Lens               (over, use, view, (%=), (+=), (^.))
import           Control.Monad.Reader.Class (ask)
import           Control.Monad.State.Class  (get, put)
import           Data.Acid                  (Query, Update, makeAcidic)
import           Data.IxSet
import           Entities.AcidDB            (AcidDB (..), cats, newestCatId)
import           Entities.Cat               (Cat (..), CatData (..), catId)

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
