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
  ( Cat(..)
  , Temperament(..)
  , Base64Picture(..)
  )
import Data.Text (Text)
import Control.Applicative 
  ( (<$>)
  , (<*>)
  , pure
  )

createCatFormlet :: (Monad m) => Form Text m Cat 
createCatFormlet = Cat Nothing
  <$> "name"        .: text Nothing
  <*> "ownerName"   .: text Nothing
  <*> "temperament" .: choice
                       [ (Friendly, "friendly")
                       , (Shy,      "shy")
                       , (Fiery,    "fiery")
                       ] Nothing
  <*> "about"       .: text Nothing
  <*> (pure (Base64Picture "hi"))
