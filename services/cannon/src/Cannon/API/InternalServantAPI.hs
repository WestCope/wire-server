{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cannon.API.InternalServantAPI where

import Cannon.App
import qualified Cannon.Dict as D
import Cannon.Types
import Cannon.WS
import Conduit
import Control.Monad.Catch
import Data.Aeson (encode)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit.List
import Data.Id hiding (client)
import Gundeck.Types
import Gundeck.Types.BulkPush
import Imports
import Servant
import Servant.Conduit ()
import System.Logger.Class (msg, val)
import qualified System.Logger.Class as LC
import Wire.API.Routes.MultiVerb

-- FUTUREWORK: This has been merged to servant upstream and can be replaced once it's released.
type HeadNoContent = NoContentVerb 'HEAD

newtype PushNotificationStream = PushNotificationStream
  { getPushNotificationStream ::
      ConduitT () ByteString (ResourceT WS) ()
  }
  deriving newtype (FromSourceIO ByteString)

type InternalServantAPI =
  -- TODO(sven): Can I teach the type system that this String is always empty?
  "i" :> "status" :> Get '[PlainText] String
    :<|> "i" :> "status" :> HeadNoContent
    :<|> "i" :> "push" :> Capture "user" UserId :> Capture "conn" ConnId :> StreamBody NoFraming OctetStream (SourceIO ByteString)
      :> MultiVerb
           'POST
           '[JSON]
           '[ Respond 410 "General error: Client gone." NoContent,
              Respond 200 "Successfully pushed." NoContent
            ]
           (Maybe NoContent)
    :<|> "i" :> "bulkpush" :> ReqBody '[JSON] BulkPushRequest :> Post '[JSON] BulkPushResponse
    :<|> "i" :> "presences" :> Capture "uid" UserId :> Capture "conn" ConnId
      :> MultiVerb
           'HEAD
           '[NoContent]
           '[ Respond 404 "Not found: Presence not registered." NoContent,
              Respond 200 "Presence checked successfully." NoContent
            ]
           (Maybe NoContent)

apiProxy :: Proxy InternalServantAPI
apiProxy = Proxy

server :: ServerT InternalServantAPI Cannon
server = pure "" :<|> pure NoContent :<|> pushHandler :<|> bulkPushHandler :<|> checkPresenceHandler

pushHandler :: UserId -> ConnId -> SourceIO ByteString -> Cannon (Maybe NoContent)
pushHandler user conn body =
  singlePush body (PushTarget user conn) >>= \case
    PushStatusOk -> pure $ Just NoContent
    PushStatusGone -> pure Nothing

-- | Take a serialized 'Notification' string and send it to the 'PushTarget'.
singlePush :: SourceIO ByteString -> PushTarget -> Cannon PushStatus
singlePush notification = singlePush' (getPushNotificationStream (fromSourceIO notification))

singlePush' :: ConduitM () ByteString (ResourceT WS) () -> PushTarget -> Cannon PushStatus
singlePush' notificationC (PushTarget usrid conid) = do
  let k = mkKey usrid conid
  d <- clients
  LC.debug $ client (key2bytes k) . msg (val "push")
  c <- D.lookup k d
  case c of
    Nothing -> do
      LC.debug $ client (key2bytes k) . msg (val "push: client gone")
      return PushStatusGone
    Just x -> do
      e <- wsenv
      runWS e $ do
        -- TODO(sven): Review streaming with Paolo. Is everthing consumed? Lazy/streaming?
        catchAll
          ( runConduitRes $
              notificationC .| (sendMsgConduit k x >> pure PushStatusOk)
          )
          (const (terminate k x >> pure PushStatusGone))

bulkPushHandler :: BulkPushRequest -> Cannon BulkPushResponse
bulkPushHandler (BulkPushRequest ns) =
  BulkPushResponse . mconcat . zipWith compileResp ns <$> (uncurry doNotify `Imports.mapM` ns)
  where
    doNotify :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotify (encode -> notification) =
      mapConcurrentlyCannon
        ( singlePush'
            (sourceLbs notification)
        )
    compileResp ::
      (Notification, [PushTarget]) ->
      [PushStatus] ->
      [(NotificationId, PushTarget, PushStatus)]
    compileResp (notif, prcs) pss = zip3 (repeat (ntfId notif)) prcs pss

checkPresenceHandler :: UserId -> ConnId -> Cannon (Maybe NoContent)
checkPresenceHandler u c = do
  e <- wsenv
  registered <- runWS e $ isRemoteRegistered u c
  if registered
    then pure $ Just NoContent
    else pure Nothing

sourceLbs :: Monad m => L.ByteString -> ConduitT i S.ByteString m ()
sourceLbs = sourceList . L.toChunks
