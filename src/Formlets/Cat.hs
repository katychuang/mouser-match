{-# LANGUAGE OverloadedStrings #-}
module Formlets.Cat
  ( catDataFormlet
  , catFormlet
  )
 where

import           Choice              (Choice (..), renderChoices)
import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Lens        (view)
import           Data.Text           (Text)
import           Entities.Cat        (Base64Picture (..), Cat (..),
                                      CatData (..), Location (..),
                                      Temperament (..), about, catData, catId,
                                      location, name, ownerName, temperament)
import           Text.Digestive      (Formlet, choice, stringRead, text, (.:))

catFormlet :: Monad m => Formlet Text m Cat
catFormlet c = Cat
  <$> "id"      .: stringRead "hi" (view catId <$> c)
  <*> "catData" .: catDataFormlet (view catData <$> c)

catDataFormlet :: Monad m => Formlet Text m CatData
catDataFormlet cd = CatData
  <$> "name"        .: text (view name <$> cd)
  <*> "ownerName"   .: text (view ownerName <$> cd)
  <*> "location"    .: choice renderChoices (view location <$> cd)
  <*> "temperament" .: choice renderChoices (view temperament <$> cd)
  <*> "about"       .: text (view about <$> cd)
  <*> (pure (Base64Picture "hi"))

instance Choice Temperament where
  renderChoice Friendly = "friendly"
  renderChoice Shy      = "shy"
  renderChoice Fiery    = "fiery"

instance Choice Location where
  renderChoice Soho               = "soho"
  renderChoice Tribeca            = "tribeca"
  renderChoice Chelsea            = "chelsea"
  renderChoice LowerEastSide      = "lower east side"
  renderChoice FortGreene         = "fort greene"
  renderChoice ProspectHeights    = "prospect heights"
  renderChoice Williamsburg       = "williamsburg"
  renderChoice Bushwick           = "bushwick"
  renderChoice CrownHeights       = "crown heights"
