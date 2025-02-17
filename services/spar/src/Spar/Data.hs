{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | Manipulating Spar records in the database.
module Spar.Data
  ( schemaVersion,
    Env (..),
    mkEnv,
    mkTTLAssertions,

    -- * SAML state handling
    storeAReqID,
    unStoreAReqID,
    isAliveAReqID,
    storeAssID,
    unStoreAssID,
    isAliveAssID,
    storeVerdictFormat,
    getVerdictFormat,

    -- * SAML Users
    NormalizedUNameID (..),
    normalizeUnqualifiedNameId,
    normalizeQualifiedNameId,
    insertSAMLUser,
    getSAMLUser,
    getSAMLAnyUserByIssuer,
    getSAMLSomeUsersByIssuer,
    deleteSAMLUsersByIssuer,
    deleteSAMLUser,

    -- * Cookies
    insertBindCookie,
    lookupBindCookie,

    -- * IDPs
    storeIdPConfig,
    Replaced (..),
    Replacing (..),
    setReplacedBy,
    clearReplacedBy,
    GetIdPResult (..),
    getIdPConfig,
    getIdPIdByIssuerWithoutTeam,
    getIdPIdByIssuerWithTeam,
    getIdPConfigsByTeam,
    deleteIdPConfig,
    storeIdPRawMetadata,
    getIdPRawMetadata,
    deleteIdPRawMetadata,

    -- * SSO settings
    storeDefaultSsoCode,
    getDefaultSsoCode,
    deleteDefaultSsoCode,

    -- * SCIM auth
    insertScimToken,
    lookupScimToken,
    getScimTokens,
    deleteScimToken,
    deleteTeamScimTokens,

    -- * SCIM externalids, user timestamps
    writeScimUserTimes,
    readScimUserTimes,
    deleteScimUserTimes,
    insertScimExternalId,
    lookupScimExternalId,
    deleteScimExternalId,
  )
where

import Brig.Types.Common (Email, fromEmail)
import Cassandra as Cas
import Control.Arrow (Arrow ((&&&)))
import Control.Lens
import Control.Monad.Except
import Data.CaseInsensitive (foldCase)
import qualified Data.CaseInsensitive as CI
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import qualified Data.List.NonEmpty as NL
import Data.String.Conversions
import Data.Time
import Data.X509 (SignedCertificate)
import GHC.TypeLits (KnownSymbol)
import Imports
import SAML2.Util (renderURI)
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.Email as SAMLEmail
import Spar.Data.Instances (VerdictFormatCon, VerdictFormatRow, fromVerdictFormat, toVerdictFormat)
import Spar.Sem.IdP (GetIdPResult (..), Replaced (..), Replacing (..))
import Text.RawString.QQ
import URI.ByteString
import qualified Web.Cookie as Cky
import Web.Scim.Schema.Common (WithId (..))
import Web.Scim.Schema.Meta (Meta (..), WithMeta (..))
import Wire.API.Cookie
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim
import qualified Prelude

-- | A lower bound: @schemaVersion <= whatWeFoundOnCassandra@, not @==@.
schemaVersion :: Int32
schemaVersion = 15

----------------------------------------------------------------------
-- helpers

-- | Carry some time constants we do not want to pull from Options, IO, respectively.  This way the
-- functions in this module need fewer effects.  See 'wrapMonadClientWithEnv' (as opposed to
-- 'wrapMonadClient' where we don't need an 'Env').
data Env = Env
  { dataEnvNow :: UTCTime,
    dataEnvMaxTTLAuthRequests :: TTL "authreq",
    dataEnvMaxTTLAssertions :: TTL "authresp"
  }
  deriving (Eq, Show)

mkEnv :: Opts -> UTCTime -> Env
mkEnv opts now =
  Env
    { dataEnvNow = now,
      dataEnvMaxTTLAuthRequests = maxttlAuthreq opts,
      dataEnvMaxTTLAssertions = maxttlAuthresp opts
    }

mkTTLAuthnRequests :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authreq")
mkTTLAuthnRequests (Env now maxttl _) = mkTTL now maxttl

mkTTLAuthnRequestsNDT :: MonadError TTLError m => Env -> NominalDiffTime -> m (TTL "authreq")
mkTTLAuthnRequestsNDT (Env _ maxttl _) = mkTTLNDT maxttl

mkTTLAssertions :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authresp")
mkTTLAssertions (Env now _ maxttl) = mkTTL now maxttl

mkTTL :: (MonadError TTLError m, KnownSymbol a) => UTCTime -> TTL a -> UTCTime -> m (TTL a)
mkTTL now maxttl endOfLife = mkTTLNDT maxttl $ endOfLife `diffUTCTime` now

mkTTLNDT :: (MonadError TTLError m, KnownSymbol a) => TTL a -> NominalDiffTime -> m (TTL a)
mkTTLNDT maxttl ttlNDT =
  if
      | actualttl > maxttl -> throwError $ TTLTooLong (showTTL actualttl) (showTTL maxttl)
      | actualttl <= 0 -> throwError $ TTLNegative (showTTL actualttl)
      | otherwise -> pure actualttl
  where
    actualttl = TTL . nominalDiffToSeconds $ ttlNDT

nominalDiffToSeconds :: NominalDiffTime -> Int32
nominalDiffToSeconds = round @Double . realToFrac

----------------------------------------------------------------------
-- saml state handling

storeAReqID ::
  (HasCallStack, MonadReader Env m, MonadClient m, MonadError TTLError m) =>
  AReqId ->
  SAML.Time ->
  m ()
storeAReqID (SAML.ID rid) (SAML.Time endOfLife) = do
  env <- ask
  TTL ttl <- mkTTLAuthnRequests env endOfLife
  retry x5 . write ins $ params LocalQuorum (rid, ttl)
  where
    ins :: PrepQuery W (SAML.XmlText, Int32) ()
    ins = "INSERT INTO authreq (req) VALUES (?) USING TTL ?"

unStoreAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m ()
unStoreAReqID (SAML.ID rid) = retry x5 . write del . params LocalQuorum $ Identity rid
  where
    del :: PrepQuery W (Identity SAML.XmlText) ()
    del = "DELETE FROM authreq WHERE req = ?"

isAliveAReqID ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m Bool
isAliveAReqID (SAML.ID rid) =
  (==) (Just 1) <$> (retry x1 . query1 sel . params LocalQuorum $ Identity rid)
  where
    sel :: PrepQuery R (Identity SAML.XmlText) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authreq WHERE req = ?"

storeAssID ::
  (HasCallStack, MonadReader Env m, MonadClient m, MonadError TTLError m) =>
  AssId ->
  SAML.Time ->
  m ()
storeAssID (SAML.ID aid) (SAML.Time endOfLife) = do
  env <- ask
  TTL ttl <- mkTTLAssertions env endOfLife
  retry x5 . write ins $ params LocalQuorum (aid, ttl)
  where
    ins :: PrepQuery W (SAML.XmlText, Int32) ()
    ins = "INSERT INTO authresp (resp) VALUES (?) USING TTL ?"

unStoreAssID ::
  (HasCallStack, MonadClient m) =>
  AssId ->
  m ()
unStoreAssID (SAML.ID aid) = retry x5 . write del . params LocalQuorum $ Identity aid
  where
    del :: PrepQuery W (Identity SAML.XmlText) ()
    del = "DELETE FROM authresp WHERE resp = ?"

isAliveAssID ::
  (HasCallStack, MonadClient m) =>
  AssId ->
  m Bool
isAliveAssID (SAML.ID aid) =
  (==) (Just 1) <$> (retry x1 . query1 sel . params LocalQuorum $ Identity aid)
  where
    sel :: PrepQuery R (Identity SAML.XmlText) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authresp WHERE resp = ?"

----------------------------------------------------------------------
-- spar state handling (not visible to saml2-web-sso)

-- | First argument is the life expectancy of the request.  (We store the verdict format for twice
-- as long.  Reason: if there is some delay in processing a very old request, it would be bad for
-- error handling if we couldn't figure out where the error will land.)
storeVerdictFormat ::
  (HasCallStack, MonadClient m) =>
  NominalDiffTime ->
  AReqId ->
  VerdictFormat ->
  m ()
storeVerdictFormat diffTime req (fromVerdictFormat -> (fmtCon, fmtMobSucc, fmtMobErr)) = do
  let ttl = nominalDiffToSeconds diffTime * 2
  retry x5 . write cql $ params LocalQuorum (req, fmtCon, fmtMobSucc, fmtMobErr, ttl)
  where
    cql :: PrepQuery W (AReqId, VerdictFormatCon, Maybe URI, Maybe URI, Int32) ()
    cql = "INSERT INTO verdict (req, format_con, format_mobile_success, format_mobile_error) VALUES (?, ?, ?, ?) USING TTL ?"

getVerdictFormat ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m (Maybe VerdictFormat)
getVerdictFormat req =
  (>>= toVerdictFormat)
    <$> (retry x1 . query1 cql $ params LocalQuorum (Identity req))
  where
    cql :: PrepQuery R (Identity AReqId) VerdictFormatRow
    cql = "SELECT format_con, format_mobile_success, format_mobile_error FROM verdict WHERE req = ?"

----------------------------------------------------------------------
-- user

-- | Used as a lookup key for 'UnqualifiedNameID' that only depends on the
-- lowercase version of the identifier. Use 'normalizeUnqualifiedNameId' or
-- 'normalizeQualifiedNameId' to create values.
newtype NormalizedUNameID = NormalizedUNameID {unNormalizedUNameID :: Text}
  deriving stock (Eq, Ord, Generic)

instance Cql NormalizedUNameID where
  ctype = Tagged TextColumn
  toCql = CqlText . unNormalizedUNameID
  fromCql (CqlText t) = pure $ NormalizedUNameID t
  fromCql _ = Left "NormalizedNameID: expected CqlText"

normalizeUnqualifiedNameId :: SAML.UnqualifiedNameID -> NormalizedUNameID
normalizeUnqualifiedNameId = NormalizedUNameID . foldCase . nameIdTxt
  where
    nameIdTxt :: SAML.UnqualifiedNameID -> ST
    nameIdTxt (SAML.UNameIDUnspecified txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDEmail email) = SAMLEmail.render $ CI.original email
    nameIdTxt (SAML.UNameIDX509 txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDWindows txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDKerberos txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDEntity uri) = renderURI uri
    nameIdTxt (SAML.UNameIDPersistent txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDTransient txt) = SAML.unsafeFromXmlText txt

-- | Qualifiers are ignored.
normalizeQualifiedNameId :: SAML.NameID -> NormalizedUNameID
normalizeQualifiedNameId = normalizeUnqualifiedNameId . view SAML.nameID

-- | Add new user.  If user with this 'SAML.UserId' exists, overwrite it.
insertSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> UserId -> m ()
insertSAMLUser (SAML.UserRef tenant subject) uid = retry x5 . write ins $ params LocalQuorum (tenant, normalizeQualifiedNameId subject, subject, uid)
  where
    ins :: PrepQuery W (SAML.Issuer, NormalizedUNameID, SAML.NameID, UserId) ()
    ins = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

-- | Sometimes we only need to know if it's none or more, so this function returns the first one.
getSAMLAnyUserByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m (Maybe UserId)
getSAMLAnyUserByIssuer issuer =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (Identity issuer))
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity UserId)
    sel = "SELECT uid FROM user_v2 WHERE issuer = ? LIMIT 1"

-- | Sometimes (eg., for IdP deletion), we can start anywhere with deleting all users in an
-- IdP, and if we don't get all users we just try again when we're done with these.
getSAMLSomeUsersByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m [(SAML.UserRef, UserId)]
getSAMLSomeUsersByIssuer issuer =
  (_1 %~ SAML.UserRef issuer)
    <$$> (retry x1 . query sel $ params LocalQuorum (Identity issuer))
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (SAML.NameID, UserId)
    sel = "SELECT sso_id, uid FROM user_v2 WHERE issuer = ? LIMIT 2000"

-- | Lookup a brig 'UserId' by IdP issuer and NameID.
--
-- NB: It is not allowed for two distinct wire users from two different teams to have the same
-- 'UserRef'.  RATIONALE: this allows us to implement 'getSAMLUser' without adding 'TeamId' to
-- 'UserRef' (which in turn would break the (admittedly leaky) abstarctions of saml2-web-sso).
getSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getSAMLUser uref = do
  mbUid <- getSAMLUserNew uref
  case mbUid of
    Nothing -> migrateLegacy uref
    Just uid -> pure $ Just uid
  where
    getSAMLUserNew :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserNew (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, normalizeQualifiedNameId subject))
      where
        sel :: PrepQuery R (SAML.Issuer, NormalizedUNameID) (Identity UserId)
        sel = "SELECT uid FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"

    migrateLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    migrateLegacy uref' = do
      mbUid <- getSAMLUserLegacy uref'
      for mbUid $ \uid -> do
        insertSAMLUser uref' uid
        pure uid

    getSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserLegacy (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, subject))
      where
        sel :: PrepQuery R (SAML.Issuer, SAML.NameID) (Identity UserId)
        sel = "SELECT uid FROM user WHERE issuer = ? AND sso_id = ?"

deleteSAMLUsersByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m ()
deleteSAMLUsersByIssuer issuer = retry x5 . write del $ params LocalQuorum (Identity issuer)
  where
    del :: PrepQuery W (Identity SAML.Issuer) ()
    del = "DELETE FROM user_v2 WHERE issuer = ?"

deleteSAMLUser :: (HasCallStack, MonadClient m) => UserId -> SAML.UserRef -> m ()
deleteSAMLUser uid uref = do
  muidUref <- getSAMLUser uref
  for_ muidUref $ \uidUref ->
    when (uidUref == uid) $ do
      deleteSAMLUserLegacy uref
      deleteSAMLUserNew uref
  where
    deleteSAMLUserNew :: (HasCallStack, MonadClient m) => SAML.UserRef -> m ()
    deleteSAMLUserNew (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, normalizeQualifiedNameId subject)
      where
        del :: PrepQuery W (SAML.Issuer, NormalizedUNameID) ()
        del = "DELETE FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"
    deleteSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m ()
    deleteSAMLUserLegacy (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, subject)
      where
        del :: PrepQuery W (SAML.Issuer, SAML.NameID) ()
        del = "DELETE FROM user WHERE issuer = ? AND sso_id = ?"

----------------------------------------------------------------------
-- bind cookies

-- | Associate the value of a 'BindCookie' with its 'UserId'.  The 'TTL' of this entry should be the
-- same as the one of the 'AuthnRequest' sent with the cookie.
insertBindCookie ::
  (HasCallStack, MonadClient m, MonadReader Env m, MonadError TTLError m) =>
  SetBindCookie ->
  UserId ->
  NominalDiffTime ->
  m ()
insertBindCookie cky uid ttlNDT = do
  env <- ask
  TTL ttlInt32 <- mkTTLAuthnRequestsNDT env ttlNDT
  let ckyval = cs . Cky.setCookieValue . SAML.fromSimpleSetCookie . getSimpleSetCookie $ cky
  retry x5 . write ins $ params LocalQuorum (ckyval, uid, ttlInt32)
  where
    ins :: PrepQuery W (ST, UserId, Int32) ()
    ins = "INSERT INTO bind_cookie (cookie, session_owner) VALUES (?, ?) USING TTL ?"

-- | The counter-part of 'insertBindCookie'.
lookupBindCookie :: (HasCallStack, MonadClient m) => BindCookie -> m (Maybe UserId)
lookupBindCookie (cs . fromBindCookie -> ckyval :: ST) =
  runIdentity <$$> do
    (retry x1 . query1 sel $ params LocalQuorum (Identity ckyval))
  where
    sel :: PrepQuery R (Identity ST) (Identity UserId)
    sel = "SELECT session_owner FROM bind_cookie WHERE cookie = ?"

----------------------------------------------------------------------
-- idp

type IdPConfigRow = (SAML.IdPId, SAML.Issuer, URI, SignedCertificate, [SignedCertificate], TeamId, Maybe WireIdPAPIVersion, [SAML.Issuer], Maybe SAML.IdPId)

-- FUTUREWORK: should be called 'insertIdPConfig' for consistency.
-- FUTUREWORK: enforce that wiReplacedby is Nothing, or throw an error.  there is no
-- legitimate reason to store an IdP that has already been replaced.  and for updating an old
-- one, call 'markReplacedIdP'.
storeIdPConfig ::
  (HasCallStack, MonadClient m) =>
  IdP ->
  m ()
storeIdPConfig idp = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  addPrepQuery
    ins
    ( idp ^. SAML.idpId,
      idp ^. SAML.idpMetadata . SAML.edIssuer,
      idp ^. SAML.idpMetadata . SAML.edRequestURI,
      NL.head (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse),
      NL.tail (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse),
      -- (the 'List1' is split up into head and tail to make migration from one-element-only easier.)
      idp ^. SAML.idpExtraInfo . wiTeam,
      idp ^. SAML.idpExtraInfo . wiApiVersion,
      idp ^. SAML.idpExtraInfo . wiOldIssuers,
      idp ^. SAML.idpExtraInfo . wiReplacedBy
    )
  addPrepQuery
    byIssuer
    ( idp ^. SAML.idpMetadata . SAML.edIssuer,
      idp ^. SAML.idpExtraInfo . wiTeam,
      idp ^. SAML.idpId
    )
  addPrepQuery
    byTeam
    ( idp ^. SAML.idpId,
      idp ^. SAML.idpExtraInfo . wiTeam
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

    -- FUTUREWORK: migrate `spar.issuer_idp` away, `spar.issuer_idp_v2` is enough.
    byIssuer :: PrepQuery W (SAML.Issuer, TeamId, SAML.IdPId) ()
    byIssuer = "INSERT INTO issuer_idp_v2 (issuer, team, idp) VALUES (?, ?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

-- | See also: test case @"{set,clear}ReplacedBy"@ in integration tests ("Test.Spar.DataSpec").
setReplacedBy ::
  (HasCallStack, MonadClient m) =>
  Replaced ->
  Replacing ->
  m ()
setReplacedBy (Replaced old) (Replacing new) = do
  retry x5 . write ins $ params LocalQuorum (new, old)
  where
    ins :: PrepQuery W (SAML.IdPId, SAML.IdPId) ()
    ins = "UPDATE idp SET replaced_by = ? WHERE idp = ?"

-- | See also: 'setReplacedBy'.
clearReplacedBy ::
  (HasCallStack, MonadClient m) =>
  Replaced ->
  m ()
clearReplacedBy (Replaced old) = do
  retry x5 . write ins $ params LocalQuorum (Identity old)
  where
    ins :: PrepQuery W (Identity SAML.IdPId) ()
    ins = "UPDATE idp SET replaced_by = null WHERE idp = ?"

getIdPConfig ::
  forall m.
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m (Maybe IdP)
getIdPConfig idpid =
  traverse toIdp =<< retry x1 (query1 sel $ params LocalQuorum (Identity idpid))
  where
    toIdp :: IdPConfigRow -> m IdP
    toIdp
      ( _idpId,
        -- metadata
        _edIssuer,
        _edRequestURI,
        certsHead,
        certsTail,
        -- extras
        teamId,
        apiVersion,
        oldIssuers,
        replacedBy
        ) = do
        let _edCertAuthnResponse = certsHead NL.:| certsTail
            _idpMetadata = SAML.IdPMetadata {..}
            _idpExtraInfo = WireIdP teamId apiVersion oldIssuers replacedBy
        pure $ SAML.IdPConfig {..}
    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by FROM idp WHERE idp = ?"

-- | Find 'IdPId' without team.  Search both `issuer_idp_v2` and `issuer_idp`; in the former,
-- make sure the result is unique (no two IdPs for two different teams).
getIdPIdByIssuerWithoutTeam ::
  (HasCallStack, MonadClient m) =>
  SAML.Issuer ->
  m (GetIdPResult SAML.IdPId)
getIdPIdByIssuerWithoutTeam issuer = do
  (runIdentity <$$> retry x1 (query selv2 $ params LocalQuorum (Identity issuer))) >>= \case
    [] ->
      (runIdentity <$$> retry x1 (query1 sel $ params LocalQuorum (Identity issuer))) >>= \case
        Just idpid -> pure $ GetIdPFound idpid
        Nothing -> pure GetIdPNotFound
    [idpid] ->
      pure $ GetIdPFound idpid
    idpids@(_ : _ : _) ->
      pure $ GetIdPNonUnique idpids
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    sel = "SELECT idp FROM issuer_idp WHERE issuer = ?"

    selv2 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selv2 = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ?"

getIdPIdByIssuerWithTeam ::
  (HasCallStack, MonadClient m) =>
  SAML.Issuer ->
  TeamId ->
  m (Maybe SAML.IdPId)
getIdPIdByIssuerWithTeam issuer tid = do
  runIdentity <$$> retry x1 (query1 sel $ params LocalQuorum (issuer, tid))
  where
    sel :: PrepQuery R (SAML.Issuer, TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ? and team = ?"

getIdPConfigsByTeam ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m [IdP]
getIdPConfigsByTeam team = do
  idpids <- runIdentity <$$> retry x1 (query sel $ params LocalQuorum (Identity team))
  catMaybes <$> mapM getIdPConfig idpids
  where
    sel :: PrepQuery R (Identity TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM team_idp WHERE team = ?"

deleteIdPConfig ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  SAML.Issuer ->
  TeamId ->
  m ()
deleteIdPConfig idp issuer team = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  addPrepQuery delDefaultIdp (Identity idp)
  addPrepQuery delIdp (Identity idp)
  addPrepQuery delIssuerIdp (Identity issuer)
  addPrepQuery delIssuerIdpV2 (Identity issuer)
  addPrepQuery delTeamIdp (team, idp)
  where
    delDefaultIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delDefaultIdp = "DELETE FROM default_idp WHERE partition_key_always_default = 'default' AND idp = ?"

    delIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delIdp = "DELETE FROM idp WHERE idp = ?"

    delIssuerIdp :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdp = "DELETE FROM issuer_idp WHERE issuer = ?"

    delIssuerIdpV2 :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdpV2 = "DELETE FROM issuer_idp_v2 WHERE issuer = ?"

    delTeamIdp :: PrepQuery W (TeamId, SAML.IdPId) ()
    delTeamIdp = "DELETE FROM team_idp WHERE team = ? and idp = ?"

storeIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  ST ->
  m ()
storeIdPRawMetadata idp raw = retry x5 . write ins $ params LocalQuorum (idp, raw)
  where
    ins :: PrepQuery W (SAML.IdPId, ST) ()
    ins = "INSERT INTO idp_raw_metadata (id, metadata) VALUES (?, ?)"

getIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m (Maybe ST)
getIdPRawMetadata idp =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (Identity idp))
  where
    sel :: PrepQuery R (Identity SAML.IdPId) (Identity ST)
    sel = "SELECT metadata FROM idp_raw_metadata WHERE id = ?"

deleteIdPRawMetadata ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m ()
deleteIdPRawMetadata idp = retry x5 . write del $ params LocalQuorum (Identity idp)
  where
    del :: PrepQuery W (Identity SAML.IdPId) ()
    del = "DELETE FROM idp_raw_metadata WHERE id = ?"

----------------------------------------------------------------------
-- SSO settings

-- It's important to maintain two invariants:
-- 1) whenever there is a default code, it must also exist in the idp table
-- 2) there can always only be one default SSO code selected

getDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  m (Maybe SAML.IdPId)
getDefaultSsoCode =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum ())
  where
    sel :: PrepQuery R () (Identity SAML.IdPId)
    sel = "SELECT idp FROM default_idp WHERE partition_key_always_default = 'default' ORDER BY idp LIMIT 1"

storeDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m ()
storeDefaultSsoCode idpId = do
  -- there is a race condition here which means there could potentially be more
  -- than one entry (violating invariant 2).
  -- However, the SELECT query will deterministally pick one of them due to the
  -- `ORDER BY` clause. The others will get removed by `deleteDefaultSsoCode`
  -- the next time this function is called (as it removes all entries).
  deleteDefaultSsoCode
  retry x5 . write ins $ params LocalQuorum (Identity idpId)
  where
    ins :: PrepQuery W (Identity SAML.IdPId) ()
    ins = "INSERT INTO default_idp (partition_key_always_default, idp) VALUES ('default', ?)"

deleteDefaultSsoCode ::
  (HasCallStack, MonadClient m) =>
  m ()
deleteDefaultSsoCode = retry x5 . write del $ params LocalQuorum ()
  where
    del :: PrepQuery W () ()
    del = "DELETE FROM default_idp WHERE partition_key_always_default = 'default'"

----------------------------------------------------------------------
-- SCIM auth
--
-- docs/developer/scim/storage.md {#DevScimStorageTokens}

type ScimTokenRow = (ScimTokenLookupKey, TeamId, ScimTokenId, UTCTime, Maybe SAML.IdPId, Text)

fromScimTokenRow :: ScimTokenRow -> ScimTokenInfo
fromScimTokenRow (_, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr) =
  ScimTokenInfo {..}

scimTokenLookupKey :: ScimTokenRow -> ScimTokenLookupKey
scimTokenLookupKey (key, _, _, _, _, _) = key

-- | Add a new SCIM provisioning token. The token should be random and
-- generated by the backend, not by the user.
insertScimToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  ScimTokenInfo ->
  m ()
insertScimToken token ScimTokenInfo {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let tokenHash = hashScimToken token
  addPrepQuery insByToken (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  addPrepQuery insByTeam (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)

insByToken, insByTeam :: PrepQuery W ScimTokenRow ()
insByToken =
  [r|
  INSERT INTO team_provisioning_by_token
    (token_, team, id, created_at, idp, descr)
    VALUES (?, ?, ?, ?, ?, ?)
  |]
insByTeam =
  [r|
  INSERT INTO team_provisioning_by_team
    (token_, team, id, created_at, idp, descr)
    VALUES (?, ?, ?, ?, ?, ?)
  |]

-- | Check whether a token exists and if yes, what team and IdP are
-- associated with it.
lookupScimToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  m (Maybe ScimTokenInfo)
lookupScimToken token = do
  let tokenHash = hashScimToken token
  rows <- retry x1 . query sel $ params LocalQuorum (tokenHash, token)
  case fmap (scimTokenLookupKey &&& Prelude.id) rows of
    [(ScimTokenLookupKeyHashed _, row)] ->
      pure (Just (fromScimTokenRow row))
    [(ScimTokenLookupKeyPlaintext plain, row)] ->
      convert plain row
    [(ScimTokenLookupKeyHashed _, _), (ScimTokenLookupKeyPlaintext plain, row)] ->
      convert plain row
    [(ScimTokenLookupKeyPlaintext plain, row), (ScimTokenLookupKeyHashed _, _)] ->
      convert plain row
    _ -> pure Nothing
  where
    sel :: PrepQuery R (ScimTokenHash, ScimToken) ScimTokenRow
    sel =
      [r|
      SELECT token_, team, id, created_at, idp, descr
        FROM team_provisioning_by_token WHERE token_ in (?, ?)
      |]

    convert :: MonadClient m => ScimToken -> ScimTokenRow -> m (Maybe ScimTokenInfo)
    convert plain row = do
      let tokenInfo = fromScimTokenRow row
      connvertPlaintextToken plain tokenInfo
      pure (Just tokenInfo)

connvertPlaintextToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  ScimTokenInfo ->
  m ()
connvertPlaintextToken token ScimTokenInfo {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let tokenHash = hashScimToken token
  -- enter by new lookup key
  addPrepQuery insByToken (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  -- update info table
  addPrepQuery insByTeam (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  -- remove old lookup key
  addPrepQuery delByTokenLookup (Identity (ScimTokenLookupKeyPlaintext token))

-- | List all tokens associated with a team, in the order of their creation.
getScimTokens ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m [ScimTokenInfo]
getScimTokens team = do
  -- We don't need pagination here because the limit should be pretty low
  -- (e.g. 16). If the limit grows, we might have to introduce pagination.
  rows <- retry x1 . query sel $ params LocalQuorum (Identity team)
  pure $ sortOn stiCreatedAt $ map fromScimTokenRow rows
  where
    sel :: PrepQuery R (Identity TeamId) ScimTokenRow
    sel =
      [r|
      SELECT token_, team, id, created_at, idp, descr
        FROM team_provisioning_by_team WHERE team = ?
      |]

-- | Delete a token.
deleteScimToken ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  ScimTokenId ->
  m ()
deleteScimToken team tokenid = do
  mbToken <- retry x1 . query1 selById $ params LocalQuorum (team, tokenid)
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery delById (team, tokenid)
    for_ mbToken $ \(Identity key) ->
      addPrepQuery delByTokenLookup (Identity key)
  where
    selById :: PrepQuery R (TeamId, ScimTokenId) (Identity ScimTokenLookupKey)
    selById =
      [r|
      SELECT token_ FROM team_provisioning_by_team
        WHERE team = ? AND id = ?
    |]

delById :: PrepQuery W (TeamId, ScimTokenId) ()
delById =
  [r|
  DELETE FROM team_provisioning_by_team
    WHERE team = ? AND id = ?
  |]

delByTokenLookup :: PrepQuery W (Identity ScimTokenLookupKey) ()
delByTokenLookup =
  [r|
  DELETE FROM team_provisioning_by_token
    WHERE token_ = ?
|]

-- | Delete all tokens belonging to a team.
deleteTeamScimTokens ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m ()
deleteTeamScimTokens team = do
  tokens <- retry x5 $ query sel $ params LocalQuorum (Identity team)
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery delByTeam (Identity team)
    mapM_ (addPrepQuery delByTokenLookup) tokens
  where
    sel :: PrepQuery R (Identity TeamId) (Identity ScimTokenLookupKey)
    sel = "SELECT token_ FROM team_provisioning_by_team WHERE team = ?"
    delByTeam :: PrepQuery W (Identity TeamId) ()
    delByTeam = "DELETE FROM team_provisioning_by_team WHERE team = ?"

----------------------------------------------------------------------
-- SCIM user records
--
-- docs/developer/scim/storage.md {#DevScimStorageUsers}

-- | Store creation and last-update time from the scim metadata under a user id.
writeScimUserTimes :: (HasCallStack, MonadClient m) => WithMeta (WithId UserId a) -> m ()
writeScimUserTimes (WithMeta meta (WithId uid _)) =
  retry x5 . write ins $
    params
      LocalQuorum
      ( uid,
        toUTCTimeMillis $ created meta,
        toUTCTimeMillis $ lastModified meta
      )
  where
    ins :: PrepQuery W (UserId, UTCTimeMillis, UTCTimeMillis) ()
    ins = "INSERT INTO scim_user_times (uid, created_at, last_updated_at) VALUES (?, ?, ?)"

-- | Read creation and last-update time from database for a given user id.
readScimUserTimes :: (HasCallStack, MonadClient m) => UserId -> m (Maybe (UTCTimeMillis, UTCTimeMillis))
readScimUserTimes uid = do
  retry x1 . query1 sel $ params LocalQuorum (Identity uid)
  where
    sel :: PrepQuery R (Identity UserId) (UTCTimeMillis, UTCTimeMillis)
    sel = "SELECT created_at, last_updated_at FROM scim_user_times WHERE uid = ?"

-- | Delete a SCIM user's access times by id.
-- You'll also want to ensure they are deleted in Brig and in the SAML Users table.
deleteScimUserTimes ::
  (HasCallStack, MonadClient m) =>
  UserId ->
  m ()
deleteScimUserTimes uid = retry x5 . write del $ params LocalQuorum (Identity uid)
  where
    del :: PrepQuery W (Identity UserId) ()
    del = "DELETE FROM scim_user_times WHERE uid = ?"

-- | If a scim externalId does not have an associated saml idp issuer, it cannot be stored in
-- table @spar.user@.  In those cases, and only in those cases, we store the mapping to
-- 'UserId' here.  (Note that since there is no associated IdP, the externalId is required to
-- be an email address, so we enforce that in the type signature, even though we only use it
-- as a 'Text'.)
insertScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> UserId -> m ()
insertScimExternalId tid (fromEmail -> email) uid =
  retry x5 . write insert $ params LocalQuorum (tid, email, uid)
  where
    insert :: PrepQuery W (TeamId, Text, UserId) ()
    insert = "INSERT INTO scim_external (team, external_id, user) VALUES (?, ?, ?)"

-- | The inverse of 'insertScimExternalId'.
lookupScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> m (Maybe UserId)
lookupScimExternalId tid (fromEmail -> email) = runIdentity <$$> (retry x1 . query1 sel $ params LocalQuorum (tid, email))
  where
    sel :: PrepQuery R (TeamId, Text) (Identity UserId)
    sel = "SELECT user FROM scim_external WHERE team = ? and external_id = ?"

-- | The other inverse of 'insertScimExternalId' :).
deleteScimExternalId :: (HasCallStack, MonadClient m) => TeamId -> Email -> m ()
deleteScimExternalId tid (fromEmail -> email) =
  retry x5 . write delete $ params LocalQuorum (tid, email)
  where
    delete :: PrepQuery W (TeamId, Text) ()
    delete = "DELETE FROM scim_external WHERE team = ? and external_id = ?"
