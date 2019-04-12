module Brig.API.Settings (putSettings) where

import Imports

import           Brig.API.Handler          (Handler)
import           Brig.App                  (mutableSettings)
import           Brig.Options              (MutableSettings, MutableSettings')
import           Control.Lens
import           Data.Barbie               (bzipWith)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai               (Response)
import           Network.Wai.Utilities     (JsonRequest, empty, parseBody', setStatus)

-- | Update the provided settings accordingly
putSettings :: JsonRequest (MutableSettings' Maybe) -> Handler Response
putSettings body = do
    newSettings <- parseBody' body
    mSettingsVar <- view mutableSettings
    atomically $ do
        mSettings <- readTVar mSettingsVar
        let mergedSettings :: MutableSettings
            mergedSettings = bzipWith fromMaybe' mSettings newSettings
        writeTVar mSettingsVar mergedSettings
    return $ (setStatus status200 empty)
  where
    fromMaybe' :: Identity a -> Maybe a -> Identity a
    fromMaybe' a ma = maybe a Identity ma
