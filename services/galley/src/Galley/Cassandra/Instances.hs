{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Galley.Cassandra.Instances
  (
  )
where

import Cassandra.CQL
import Control.Error (note)
import Data.Domain (Domain, domainText, mkDomain)
import qualified Data.Set as Set
import Galley.Types
import Galley.Types.Bot ()
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Imports
import qualified Wire.API.Team.Feature as Public

deriving instance Cql MutedStatus

deriving instance Cql ReceiptMode

instance Cql ConvType where
  ctype = Tagged IntColumn

  toCql RegularConv = CqlInt 0
  toCql SelfConv = CqlInt 1
  toCql One2OneConv = CqlInt 2
  toCql ConnectConv = CqlInt 3

  fromCql (CqlInt i) = case i of
    0 -> return RegularConv
    1 -> return SelfConv
    2 -> return One2OneConv
    3 -> return ConnectConv
    n -> Left $ "unexpected conversation-type: " ++ show n
  fromCql _ = Left "conv-type: int expected"

instance Cql Access where
  ctype = Tagged IntColumn

  toCql PrivateAccess = CqlInt 1
  toCql InviteAccess = CqlInt 2
  toCql LinkAccess = CqlInt 3
  toCql CodeAccess = CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> return PrivateAccess
    2 -> return InviteAccess
    3 -> return LinkAccess
    4 -> return CodeAccess
    n -> Left $ "Unexpected Access value: " ++ show n
  fromCql _ = Left "Access value: int expected"

instance Cql FromAccessRoleLegacy where
  ctype = Tagged IntColumn

  -- todo(leif): can we make this total?
  toCql (FromAccessRoleLegacy accessRoles)
    | Set.null accessRoles = CqlInt 1
    | accessRoles == Set.fromList [TeamMemberAccessRole] = CqlInt 2
    | accessRoles == Set.fromList [GuestAccessRole] = CqlInt 4
    | accessRoles == Set.fromList [ServiceAccessRole] = CqlInt 4
    | accessRoles == Set.fromList [TeamMemberAccessRole, GuestAccessRole] = CqlInt 4
    | accessRoles == Set.fromList [TeamMemberAccessRole, ServiceAccessRole] = CqlInt 4
    | accessRoles == Set.fromList [GuestAccessRole, ServiceAccessRole] = CqlInt 4
    | accessRoles == Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole] = CqlInt 4
    | otherwise = CqlInt 1 -- can we avoid a catch all case?

  fromCql (CqlInt i) = case i of
    1 -> return $ FromAccessRoleLegacy (Set.fromList [])
    2 -> return $ FromAccessRoleLegacy (Set.fromList [TeamMemberAccessRole])
    3 -> return $ FromAccessRoleLegacy (Set.fromList [TeamMemberAccessRole, GuestAccessRole])
    4 -> return $ FromAccessRoleLegacy (Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
    n -> Left $ "Unexpected AccessRoleLegacy value: " ++ show n
  fromCql _ = Left "AccessRoleLegacy value: int expected"

instance Cql AccessRoleV2 where
  ctype = Tagged IntColumn

  toCql = \case
    TeamMemberAccessRole -> CqlInt 1
    GuestAccessRole -> CqlInt 2
    ServiceAccessRole -> CqlInt 3

  fromCql (CqlInt i) = case i of
    1 -> return TeamMemberAccessRole
    2 -> return GuestAccessRole
    3 -> return ServiceAccessRole
    n -> Left $ "Unexpected AccessRoleV2 value: " ++ show n
  fromCql _ = Left "AccessRoleV2 value: int expected"

instance Cql ConvTeamInfo where
  ctype = Tagged $ UdtColumn "teaminfo" [("teamid", UuidColumn), ("managed", BooleanColumn)]

  toCql t = CqlUdt [("teamid", toCql (cnvTeamId t)), ("managed", toCql (cnvManaged t))]

  fromCql (CqlUdt u) = do
    t <- note "missing 'teamid' in teaminfo" ("teamid" `lookup` u) >>= fromCql
    m <- note "missing 'managed' in teaminfo" ("managed" `lookup` u) >>= fromCql
    pure (ConvTeamInfo t m)
  fromCql _ = Left "teaminfo: udt expected"

instance Cql TeamBinding where
  ctype = Tagged BooleanColumn

  toCql Binding = CqlBoolean True
  toCql NonBinding = CqlBoolean False

  fromCql (CqlBoolean True) = pure Binding
  fromCql (CqlBoolean False) = pure NonBinding
  fromCql _ = Left "teambinding: boolean expected"

instance Cql TeamStatus where
  ctype = Tagged IntColumn

  toCql Active = CqlInt 0
  toCql PendingDelete = CqlInt 1
  toCql Deleted = CqlInt 2
  toCql Suspended = CqlInt 3
  toCql PendingActive = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> return Active
    1 -> return PendingDelete
    2 -> return Deleted
    3 -> return Suspended
    4 -> return PendingActive
    n -> Left $ "unexpected team-status: " ++ show n
  fromCql _ = Left "team-status: int expected"

instance Cql TeamSearchVisibility where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ SearchVisibilityStandard
    1 -> pure $ SearchVisibilityNoNameOutsideTeam
    _ -> Left "fromCql: Invalid TeamSearchVisibility"
  fromCql _ = Left "fromCql: TeamSearchVisibility: CqlInt expected"

  toCql SearchVisibilityStandard = CqlInt 0
  toCql SearchVisibilityNoNameOutsideTeam = CqlInt 1

instance Cql Domain where
  ctype = Tagged TextColumn
  toCql = CqlText . domainText
  fromCql (CqlText txt) = mkDomain txt
  fromCql _ = Left "Domain: Text expected"

instance Cql Public.EnforceAppLock where
  ctype = Tagged IntColumn
  toCql (Public.EnforceAppLock False) = CqlInt 0
  toCql (Public.EnforceAppLock True) = CqlInt 1
  fromCql (CqlInt n) = case n of
    0 -> pure (Public.EnforceAppLock False)
    1 -> pure (Public.EnforceAppLock True)
    _ -> Left "fromCql EnforceAppLock: int out of range"
  fromCql _ = Left "fromCql EnforceAppLock: int expected"
