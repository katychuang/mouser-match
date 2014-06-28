module Forms.Cat
  ( catDataForm
  , catForm
  )
 where

import Text.Digestive
  ( (.:)
  , choice
  , text
  , Form
  , stringRead
  )
import Entities.Cat
  ( CatData(..)
  , Cat(..)
  , Temperament(..)
  , Base64Picture(..)
  , name
  , ownerName
  , temperament
  , catId
  , catData
  , about
  )
import Data.Text (Text)
import Control.Applicative 
  ( (<$>)
  , (<*>)
  , pure
  )
import Control.Lens 
 ( view
 )


catForm :: (Monad m) => Maybe Cat -> Form Text m Cat
catForm c = Cat
  <$> "id"      .: stringRead "hi" (view catId <$> c)
  <*> "catData" .: catDataForm (view catData <$> c)

catDataForm :: (Monad m) => Maybe CatData -> Form Text m CatData
catDataForm cd = CatData
  <$> "name"        .: text (view name <$> cd)
  <*> "ownerName"   .: text (view ownerName <$> cd)
  <*> "temperament" .: choice
                       [ (Friendly, "friendly")
                       , (Shy,      "shy")
                       , (Fiery,    "fiery")
                       ] (view temperament <$> cd)
  <*> "about"       .: text (view about <$> cd)
  <*> (pure (Base64Picture "hi"))
