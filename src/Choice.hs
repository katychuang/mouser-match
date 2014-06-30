module Choice
  ( Choice(..)
  , renderChoices
  )
 where

import           Data.Text (Text)

class Choice a where
  renderChoice :: a -> Text

renderChoices :: (Choice a, Enum a) => [(a, Text)]
renderChoices = map (\x -> (x,renderChoice x)) [toEnum 0..]
