module Formlets.Cat.Create
  ( createCatFormlet
  )
 where

import Text.Digestive
  ( (.:)
  , choice
  , text
  , Form
  )
import Entities.Cat
  ( CatData(..)
  , Temperament(..)
  , Base64Picture(..)
  )
import Data.Text (Text)
import Control.Applicative 
  ( (<$>)
  , (<*>)
  , pure
  )

createCatFormlet :: (Monad m) => Form Text m CatData
createCatFormlet = CatData
  <$> "name"        .: text Nothing
  <*> "ownerName"   .: text Nothing
  <*> "temperament" .: choice
                       [ (Friendly, "friendly")
                       , (Shy,      "shy")
                       , (Fiery,    "fiery")
                       ] Nothing
  <*> "about"       .: text Nothing
  <*> (pure (Base64Picture "hi"))
