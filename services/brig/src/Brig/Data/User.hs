{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- for Show UserRowInsert

-- TODO: Move to Brig.User.Account.DB
module Brig.Data.User
  ( AuthError (..),
    ReAuthError (..),
    newAccount,
    newAccountInviteViaScim,
    insertAccount,
    authenticate,
    reauthenticate,
    filterActive,
    isActivated,

    -- * Lookups
    lookupAccount,
    lookupAccounts,
    lookupUser,
    lookupUsers,
    lookupName,
    lookupLocale,
    lookupPassword,
    lookupStatus,
    lookupRichInfo,
    lookupRichInfoMultiUsers,
    lookupUserTeam,
    lookupServiceUsers,
    lookupServiceUsersForTeam,
    lookupFeatureConferenceCalling,

    -- * Updates
    updateUser,
    updateEmail,
    updatePhone,
    updateSSOId,
    updateManagedBy,
    activateUser,
    deactivateUser,
    updateLocale,
    updatePassword,
    updateStatus,
    updateHandle,
    updateRichInfo,
    updateFeatureConferenceCalling,

    -- * Deletions
    deleteEmail,
    deletePhone,
    deleteServiceUser,
  )
where

import Brig.App (AppIO, currentTime, settings, viewFederationDomain, zauthEnv)
import Brig.Data.Instances ()
import Brig.Options
import Brig.Password
import Brig.Types
import Brig.Types.Intra
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error
import Control.Lens hiding (from)
import Data.Conduit (ConduitM)
import Data.Domain
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.UUID.V4
import Galley.Types.Bot
import Imports
import qualified Wire.API.Team.Feature as ApiFt
import Wire.API.User.RichInfo

-- | Authentication errors.
data AuthError
  = AuthInvalidUser
  | AuthInvalidCredentials
  | AuthSuspended
  | AuthEphemeral
  | AuthPendingInvitation

-- | Re-authentication errors.
data ReAuthError
  = ReAuthError !AuthError
  | ReAuthMissingPassword

-- | Preconditions:
--
-- 1. @newUserUUID u == Just inv || isNothing (newUserUUID u)@.
-- 2. If @isJust@, @mbHandle@ must be claimed by user with id @inv@.
--
-- Condition (2.) is essential for maintaining handle uniqueness.  It is guaranteed by the
-- fact that we're setting getting @mbHandle@ from table @"user"@, and when/if it was added
-- there, it was claimed properly.
newAccount :: NewUser -> Maybe InvitationId -> Maybe TeamId -> Maybe Handle -> AppIO (UserAccount, Maybe Password)
newAccount u inv tid mbHandle = do
  defLoc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  uid <-
    Id <$> do
      case (inv, newUserUUID u) of
        (Just (toUUID -> uuid), _) -> pure uuid
        (_, Just uuid) -> pure uuid
        (Nothing, Nothing) -> liftIO nextRandom
  passwd <- maybe (return Nothing) (fmap Just . liftIO . mkSafePassword) pass
  expiry <- case status of
    Ephemeral -> do
      -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
      e <- view zauthEnv
      let ZAuth.SessionTokenTimeout defTTL = e ^. ZAuth.settings . ZAuth.sessionTokenTimeout
          ttl = maybe defTTL fromRange (newUserExpiresIn u)
      now <- liftIO =<< view currentTime
      return . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
    _ -> return Nothing
  return (UserAccount (user uid domain (locale defLoc) expiry) status, passwd)
  where
    ident = newUserIdentity u
    pass = newUserPassword u
    name = newUserDisplayName u
    pict = fromMaybe noPict (newUserPict u)
    assets = newUserAssets u
    status =
      if isNewUserEphemeral u
        then Ephemeral
        else Active
    colour = fromMaybe defaultAccentId (newUserAccentId u)
    locale defLoc = fromMaybe defLoc (newUserLocale u)
    managedBy = fromMaybe defaultManagedBy (newUserManagedBy u)
    user uid domain l e = User uid (Qualified uid domain) ident name pict assets colour False l Nothing mbHandle e tid managedBy

newAccountInviteViaScim :: UserId -> TeamId -> Maybe Locale -> Name -> Email -> AppIO UserAccount
newAccountInviteViaScim uid tid locale name email = do
  defLoc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  return (UserAccount (user domain (fromMaybe defLoc locale)) PendingInvitation)
  where
    user domain loc =
      User
        uid
        (Qualified uid domain)
        (Just $ EmailIdentity email)
        name
        (Pict [])
        []
        defaultAccentId
        False
        loc
        Nothing
        Nothing
        Nothing
        (Just tid)
        ManagedByScim

-- | Mandatory password authentication.
authenticate :: UserId -> PlainTextPassword -> ExceptT AuthError AppIO ()
authenticate u pw =
  lift (lookupAuth u) >>= \case
    Nothing -> throwE AuthInvalidUser
    Just (_, Deleted) -> throwE AuthInvalidUser
    Just (_, Suspended) -> throwE AuthSuspended
    Just (_, Ephemeral) -> throwE AuthEphemeral
    Just (_, PendingInvitation) -> throwE AuthPendingInvitation
    Just (Nothing, _) -> throwE AuthInvalidCredentials
    Just (Just pw', Active) ->
      unless (verifyPassword pw pw') $
        throwE AuthInvalidCredentials

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If the account has no password and no password is given,
-- reauthentication is a no-op.
reauthenticate :: (MonadClient m) => UserId -> Maybe PlainTextPassword -> ExceptT ReAuthError m ()
reauthenticate u pw =
  lift (lookupAuth u) >>= \case
    Nothing -> throwE (ReAuthError AuthInvalidUser)
    Just (_, Deleted) -> throwE (ReAuthError AuthInvalidUser)
    Just (_, Suspended) -> throwE (ReAuthError AuthSuspended)
    Just (_, PendingInvitation) -> throwE (ReAuthError AuthPendingInvitation)
    Just (Nothing, _) -> for_ pw $ const (throwE $ ReAuthError AuthInvalidCredentials)
    Just (Just pw', Active) -> maybeReAuth pw'
    Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    maybeReAuth pw' = case pw of
      Nothing -> throwE ReAuthMissingPassword
      Just p ->
        unless (verifyPassword p pw') $
          throwE (ReAuthError AuthInvalidCredentials)

insertAccount ::
  UserAccount ->
  -- | If a bot: conversation and team
  --   (if a team conversation)
  Maybe (ConvId, Maybe TeamId) ->
  Maybe Password ->
  -- | Whether the user is activated
  Bool ->
  AppIO ()
insertAccount (UserAccount u status) mbConv password activated = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let Locale l c = userLocale u
  addPrepQuery
    userInsert
    ( userId u,
      userDisplayName u,
      userPict u,
      userAssets u,
      userEmail u,
      userPhone u,
      userSSOId u,
      userAccentId u,
      password,
      activated,
      status,
      userExpire u,
      l,
      c,
      view serviceRefProvider <$> userService u,
      view serviceRefId <$> userService u,
      userHandle u,
      userTeam u,
      userManagedBy u
    )
  for_ ((,) <$> userService u <*> mbConv) $ \(sref, (cid, mbTid)) -> do
    let pid = sref ^. serviceRefProvider
        sid = sref ^. serviceRefId
    addPrepQuery cqlServiceUser (pid, sid, BotId (userId u), cid, mbTid)
    for_ mbTid $ \tid ->
      addPrepQuery cqlServiceTeam (pid, sid, BotId (userId u), cid, tid)
  where
    cqlServiceUser :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, Maybe TeamId) ()
    cqlServiceUser =
      "INSERT INTO service_user (provider, service, user, conv, team) \
      \VALUES (?, ?, ?, ?, ?)"
    cqlServiceTeam :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, TeamId) ()
    cqlServiceTeam =
      "INSERT INTO service_team (provider, service, user, conv, team) \
      \VALUES (?, ?, ?, ?, ?)"

updateLocale :: UserId -> Locale -> AppIO ()
updateLocale u (Locale l c) = write userLocaleUpdate (params LocalQuorum (l, c, u))

updateUser :: UserId -> UserUpdate -> AppIO ()
updateUser u UserUpdate {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ uupName $ \n -> addPrepQuery userDisplayNameUpdate (n, u)
  for_ uupPict $ \p -> addPrepQuery userPictUpdate (p, u)
  for_ uupAssets $ \a -> addPrepQuery userAssetsUpdate (a, u)
  for_ uupAccentId $ \c -> addPrepQuery userAccentIdUpdate (c, u)

updateEmail :: UserId -> Email -> AppIO ()
updateEmail u e = retry x5 $ write userEmailUpdate (params LocalQuorum (e, u))

updatePhone :: UserId -> Phone -> AppIO ()
updatePhone u p = retry x5 $ write userPhoneUpdate (params LocalQuorum (p, u))

updateSSOId :: UserId -> Maybe UserSSOId -> AppIO Bool
updateSSOId u ssoid = do
  mteamid <- lookupUserTeam u
  case mteamid of
    Just _ -> do
      retry x5 $ write userSSOIdUpdate (params LocalQuorum (ssoid, u))
      pure True
    Nothing -> pure False

updateManagedBy :: UserId -> ManagedBy -> AppIO ()
updateManagedBy u h = retry x5 $ write userManagedByUpdate (params LocalQuorum (h, u))

updateHandle :: UserId -> Handle -> AppIO ()
updateHandle u h = retry x5 $ write userHandleUpdate (params LocalQuorum (h, u))

updatePassword :: UserId -> PlainTextPassword -> AppIO ()
updatePassword u t = do
  p <- liftIO $ mkSafePassword t
  retry x5 $ write userPasswordUpdate (params LocalQuorum (p, u))

updateRichInfo :: UserId -> RichInfoAssocList -> AppIO ()
updateRichInfo u ri = retry x5 $ write userRichInfoUpdate (params LocalQuorum (ri, u))

updateFeatureConferenceCalling :: UserId -> Maybe ApiFt.TeamFeatureStatusNoConfig -> AppIO (Maybe ApiFt.TeamFeatureStatusNoConfig)
updateFeatureConferenceCalling uid mbStatus = do
  let flag = ApiFt.tfwoStatus <$> mbStatus
  retry x5 $ write update (params LocalQuorum (flag, uid))
  pure mbStatus
  where
    update :: PrepQuery W (Maybe ApiFt.TeamFeatureStatusValue, UserId) ()
    update = fromString $ "update user set feature_conference_calling = ? where id = ?"

deleteEmail :: UserId -> AppIO ()
deleteEmail u = retry x5 $ write userEmailDelete (params LocalQuorum (Identity u))

deletePhone :: UserId -> AppIO ()
deletePhone u = retry x5 $ write userPhoneDelete (params LocalQuorum (Identity u))

deleteServiceUser :: ProviderId -> ServiceId -> BotId -> AppIO ()
deleteServiceUser pid sid bid = do
  lookupServiceUser pid sid bid >>= \case
    Nothing -> pure ()
    Just (_, mbTid) -> retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery cql (pid, sid, bid)
      for_ mbTid $ \tid ->
        addPrepQuery cqlTeam (pid, sid, tid, bid)
  where
    cql :: PrepQuery W (ProviderId, ServiceId, BotId) ()
    cql =
      "DELETE FROM service_user \
      \WHERE provider = ? AND service = ? AND user = ?"
    cqlTeam :: PrepQuery W (ProviderId, ServiceId, TeamId, BotId) ()
    cqlTeam =
      "DELETE FROM service_team \
      \WHERE provider = ? AND service = ? AND team = ? AND user = ?"

updateStatus :: UserId -> AccountStatus -> AppIO ()
updateStatus u s = retry x5 $ write userStatusUpdate (params LocalQuorum (s, u))

-- | Whether the account has been activated by verifying
-- an email address or phone number.
isActivated :: UserId -> AppIO Bool
isActivated u =
  (== Just (Identity True))
    <$> retry x1 (query1 activatedSelect (params LocalQuorum (Identity u)))

filterActive :: [UserId] -> AppIO [UserId]
filterActive us =
  map (view _1) . filter isActiveUser
    <$> retry x1 (query accountStateSelectAll (params LocalQuorum (Identity us)))
  where
    isActiveUser :: (UserId, Bool, Maybe AccountStatus) -> Bool
    isActiveUser (_, True, Just Active) = True
    isActiveUser _ = False

lookupUser :: HavePendingInvitations -> UserId -> AppIO (Maybe User)
lookupUser hpi u = listToMaybe <$> lookupUsers hpi [u]

activateUser :: UserId -> UserIdentity -> AppIO ()
activateUser u ident = do
  let email = emailIdentity ident
  let phone = phoneIdentity ident
  retry x5 $ write userActivatedUpdate (params LocalQuorum (email, phone, u))

deactivateUser :: UserId -> AppIO ()
deactivateUser u =
  retry x5 $ write userDeactivatedUpdate (params LocalQuorum (Identity u))

lookupLocale :: UserId -> AppIO (Maybe Locale)
lookupLocale u = do
  defLoc <- setDefaultUserLocale <$> view settings
  fmap (toLocale defLoc) <$> retry x1 (query1 localeSelect (params LocalQuorum (Identity u)))

lookupName :: UserId -> AppIO (Maybe Name)
lookupName u =
  fmap runIdentity
    <$> retry x1 (query1 nameSelect (params LocalQuorum (Identity u)))

lookupPassword :: UserId -> AppIO (Maybe Password)
lookupPassword u =
  join . fmap runIdentity
    <$> retry x1 (query1 passwordSelect (params LocalQuorum (Identity u)))

lookupStatus :: UserId -> AppIO (Maybe AccountStatus)
lookupStatus u =
  join . fmap runIdentity
    <$> retry x1 (query1 statusSelect (params LocalQuorum (Identity u)))

lookupRichInfo :: UserId -> AppIO (Maybe RichInfoAssocList)
lookupRichInfo u =
  fmap runIdentity
    <$> retry x1 (query1 richInfoSelect (params LocalQuorum (Identity u)))

-- | Returned rich infos are in the same order as users
lookupRichInfoMultiUsers :: [UserId] -> AppIO [(UserId, RichInfo)]
lookupRichInfoMultiUsers users = do
  mapMaybe (\(uid, mbRi) -> (uid,) . RichInfo <$> mbRi)
    <$> retry x1 (query richInfoSelectMulti (params LocalQuorum (Identity users)))

-- | Lookup user (no matter what status) and return 'TeamId'.  Safe to use for authorization:
-- suspended / deleted / ... users can't login, so no harm done if we authorize them *after*
-- successful login.
lookupUserTeam :: UserId -> AppIO (Maybe TeamId)
lookupUserTeam u =
  (runIdentity =<<)
    <$> retry x1 (query1 teamSelect (params LocalQuorum (Identity u)))

lookupAuth :: (MonadClient m) => UserId -> m (Maybe (Maybe Password, AccountStatus))
lookupAuth u = fmap f <$> retry x1 (query1 authSelect (params LocalQuorum (Identity u)))
  where
    f (pw, st) = (pw, fromMaybe Active st)

-- | Return users with given IDs.
--
-- Skips nonexistent users. /Does not/ skip users who have been deleted.
lookupUsers :: HavePendingInvitations -> [UserId] -> AppIO [User]
lookupUsers hpi usrs = do
  loc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  toUsers domain loc hpi <$> retry x1 (query usersSelect (params LocalQuorum (Identity usrs)))

lookupAccount :: UserId -> AppIO (Maybe UserAccount)
lookupAccount u = listToMaybe <$> lookupAccounts [u]

lookupAccounts :: [UserId] -> AppIO [UserAccount]
lookupAccounts usrs = do
  loc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  fmap (toUserAccount domain loc) <$> retry x1 (query accountsSelect (params LocalQuorum (Identity usrs)))

lookupServiceUser :: ProviderId -> ServiceId -> BotId -> AppIO (Maybe (ConvId, Maybe TeamId))
lookupServiceUser pid sid bid = retry x1 (query1 cql (params LocalQuorum (pid, sid, bid)))
  where
    cql :: PrepQuery R (ProviderId, ServiceId, BotId) (ConvId, Maybe TeamId)
    cql =
      "SELECT conv, team FROM service_user \
      \WHERE provider = ? AND service = ? AND user = ?"

-- | NB: might return a lot of users, and therefore we do streaming here (page-by-page).
lookupServiceUsers ::
  ProviderId ->
  ServiceId ->
  ConduitM () [(BotId, ConvId, Maybe TeamId)] AppIO ()
lookupServiceUsers pid sid =
  paginateC cql (paramsP LocalQuorum (pid, sid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (BotId, ConvId, Maybe TeamId)
    cql =
      "SELECT user, conv, team FROM service_user \
      \WHERE provider = ? AND service = ?"

lookupServiceUsersForTeam ::
  ProviderId ->
  ServiceId ->
  TeamId ->
  ConduitM () [(BotId, ConvId)] AppIO ()
lookupServiceUsersForTeam pid sid tid =
  paginateC cql (paramsP LocalQuorum (pid, sid, tid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId, TeamId) (BotId, ConvId)
    cql =
      "SELECT user, conv FROM service_team \
      \WHERE provider = ? AND service = ? AND team = ?"

lookupFeatureConferenceCalling :: MonadClient m => UserId -> m (Maybe ApiFt.TeamFeatureStatusNoConfig)
lookupFeatureConferenceCalling uid = do
  let q = query1 select (params LocalQuorum (Identity uid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ ApiFt.TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: PrepQuery R (Identity UserId) (Identity (Maybe ApiFt.TeamFeatureStatusValue))
    select = fromString "select feature_conference_calling from user where id = ?"

-------------------------------------------------------------------------------
-- Queries

type Activated = Bool

type UserRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

type UserRowInsert =
  ( UserId,
    Name,
    Pict,
    [Asset],
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe Password,
    Activated,
    AccountStatus,
    Maybe UTCTimeMillis,
    Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    ManagedBy
  )

deriving instance Show UserRowInsert

-- Represents a 'UserAccount'
type AccountRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

usersSelect :: PrepQuery R (Identity [UserId]) UserRow
usersSelect =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by \
  \FROM user where id IN ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

localeSelect :: PrepQuery R (Identity UserId) (Maybe Language, Maybe Country)
localeSelect = "SELECT language, country FROM user WHERE id = ?"

authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
authSelect = "SELECT password, status FROM user WHERE id = ?"

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

activatedSelect :: PrepQuery R (Identity UserId) (Identity Bool)
activatedSelect = "SELECT activated FROM user WHERE id = ?"

accountStateSelectAll :: PrepQuery R (Identity [UserId]) (UserId, Bool, Maybe AccountStatus)
accountStateSelectAll = "SELECT id, activated, status FROM user WHERE id IN ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

richInfoSelect :: PrepQuery R (Identity UserId) (Identity RichInfoAssocList)
richInfoSelect = "SELECT json FROM rich_info WHERE user = ?"

richInfoSelectMulti :: PrepQuery R (Identity [UserId]) (UserId, Maybe RichInfoAssocList)
richInfoSelectMulti = "SELECT user, json FROM rich_info WHERE user in ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

accountsSelect :: PrepQuery R (Identity [UserId]) AccountRow
accountsSelect =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, \
  \service, handle, team, managed_by \
  \FROM user WHERE id IN ?"

userInsert :: PrepQuery W UserRowInsert ()
userInsert =
  "INSERT INTO user (id, name, picture, assets, email, phone, sso_id, \
  \accent_id, password, activated, status, expires, language, \
  \country, provider, service, handle, team, managed_by) \
  \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

userDisplayNameUpdate :: PrepQuery W (Name, UserId) ()
userDisplayNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

userPictUpdate :: PrepQuery W (Pict, UserId) ()
userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"

userEmailUpdate :: PrepQuery W (Email, UserId) ()
userEmailUpdate = "UPDATE user SET email = ? WHERE id = ?"

userPhoneUpdate :: PrepQuery W (Phone, UserId) ()
userPhoneUpdate = "UPDATE user SET phone = ? WHERE id = ?"

userSSOIdUpdate :: PrepQuery W (Maybe UserSSOId, UserId) ()
userSSOIdUpdate = "UPDATE user SET sso_id = ? WHERE id = ?"

userManagedByUpdate :: PrepQuery W (ManagedBy, UserId) ()
userManagedByUpdate = "UPDATE user SET managed_by = ? WHERE id = ?"

userHandleUpdate :: PrepQuery W (Handle, UserId) ()
userHandleUpdate = "UPDATE user SET handle = ? WHERE id = ?"

userPasswordUpdate :: PrepQuery W (Password, UserId) ()
userPasswordUpdate = "UPDATE user SET password = ? WHERE id = ?"

userStatusUpdate :: PrepQuery W (AccountStatus, UserId) ()
userStatusUpdate = "UPDATE user SET status = ? WHERE id = ?"

userDeactivatedUpdate :: PrepQuery W (Identity UserId) ()
userDeactivatedUpdate = "UPDATE user SET activated = false WHERE id = ?"

userActivatedUpdate :: PrepQuery W (Maybe Email, Maybe Phone, UserId) ()
userActivatedUpdate = "UPDATE user SET activated = true, email = ?, phone = ? WHERE id = ?"

userLocaleUpdate :: PrepQuery W (Language, Maybe Country, UserId) ()
userLocaleUpdate = "UPDATE user SET language = ?, country = ? WHERE id = ?"

userEmailDelete :: PrepQuery W (Identity UserId) ()
userEmailDelete = "UPDATE user SET email = null WHERE id = ?"

userPhoneDelete :: PrepQuery W (Identity UserId) ()
userPhoneDelete = "UPDATE user SET phone = null WHERE id = ?"

userRichInfoUpdate :: PrepQuery W (RichInfoAssocList, UserId) ()
userRichInfoUpdate = "UPDATE rich_info SET json = ? WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

-- | Construct a 'UserAccount' from a raw user record in the database.
toUserAccount :: Domain -> Locale -> AccountRow -> UserAccount
toUserAccount
  domain
  defaultLocale
  ( uid,
    name,
    pict,
    email,
    phone,
    ssoid,
    accent,
    assets,
    activated,
    status,
    expires,
    lan,
    con,
    pid,
    sid,
    handle,
    tid,
    managed_by
    ) =
    let ident = toIdentity activated email phone ssoid
        deleted = maybe False (== Deleted) status
        expiration = if status == Just Ephemeral then expires else Nothing
        loc = toLocale defaultLocale (lan, con)
        svc = newServiceRef <$> sid <*> pid
     in UserAccount
          ( User
              uid
              (Qualified uid domain)
              ident
              name
              (fromMaybe noPict pict)
              (fromMaybe [] assets)
              accent
              deleted
              loc
              svc
              handle
              expiration
              tid
              (fromMaybe ManagedByWire managed_by)
          )
          (fromMaybe Active status)

toUsers :: Domain -> Locale -> HavePendingInvitations -> [UserRow] -> [User]
toUsers domain defaultLocale havePendingInvitations = fmap mk . filter fp
  where
    fp :: UserRow -> Bool
    fp =
      case havePendingInvitations of
        WithPendingInvitations -> const True
        NoPendingInvitations ->
          ( \( _uid,
               _name,
               _pict,
               _email,
               _phone,
               _ssoid,
               _accent,
               _assets,
               _activated,
               status,
               _expires,
               _lan,
               _con,
               _pid,
               _sid,
               _handle,
               _tid,
               _managed_by
               ) -> status /= Just PendingInvitation
          )

    mk :: UserRow -> User
    mk
      ( uid,
        name,
        pict,
        email,
        phone,
        ssoid,
        accent,
        assets,
        activated,
        status,
        expires,
        lan,
        con,
        pid,
        sid,
        handle,
        tid,
        managed_by
        ) =
        let ident = toIdentity activated email phone ssoid
            deleted = maybe False (== Deleted) status
            expiration = if status == Just Ephemeral then expires else Nothing
            loc = toLocale defaultLocale (lan, con)
            svc = newServiceRef <$> sid <*> pid
         in User
              uid
              (Qualified uid domain)
              ident
              name
              (fromMaybe noPict pict)
              (fromMaybe [] assets)
              accent
              deleted
              loc
              svc
              handle
              expiration
              tid
              (fromMaybe ManagedByWire managed_by)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _ = l

-- | Construct a 'UserIdentity'.
--
-- If the user is not activated, 'toIdentity' will return 'Nothing' as a precaution, because
-- elsewhere we rely on the fact that a non-empty 'UserIdentity' means that the user is
-- activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that having an
-- email or phone in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe Email ->
  Maybe Phone ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) (Just p) Nothing = Just $! FullIdentity e p
toIdentity True (Just e) Nothing Nothing = Just $! EmailIdentity e
toIdentity True Nothing (Just p) Nothing = Just $! PhoneIdentity p
toIdentity True email phone (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True Nothing Nothing Nothing = Nothing
toIdentity False _ _ _ = Nothing
