{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState(Acid, HasAcid, getAcidStore)
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.ByteString
import Control.Applicative
import Data.ByteString
import Data.Text
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class

data Temperament = Friendly 
                 | Shy
                 | Fiery
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Temperament)

newtype Base64Picture = Base64Picture ByteString
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Base64Picture)

data Cat = Cat
  { _name        :: Text
  , _ownerName   :: Text
  , _temperament :: Temperament
  , _about       :: Text
  , _picture     :: Base64Picture
  }
  deriving(Data, Typeable, Show, Eq)
$(deriveSafeCopy 0 'base ''Cat)

instance Monoid Cat where
  mempty = Cat "" "" Friendly "" (Base64Picture "")
  mappend x y = 
    if (x == mempty)
      then y
      else x
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _acidState :: Snaplet (Acid Cat)
    }

makeLenses ''App
makeLenses ''Cat

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App




newCat :: Cat -> Update Cat ()
newCat c = put c
  
getCat :: Query Cat Cat
getCat = ask

$(makeAcidic ''Cat ['newCat, 'getCat])
instance HasAcid App Cat where
  getAcidStore x = view snapletValue (view acidState x)
