{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley where

import qualified Data.Code as Code
import Data.CommaSeparatedList
import Data.Id (ConvId, TeamId, UserId)
import Data.Qualified (Qualified (..))
import Data.Range
import Data.SOP
import qualified Data.Swagger as Swagger
import GHC.TypeLits (AppendSymbol)
import Imports hiding (head)
import Servant
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.ErrorDescription
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.ServantProto (Proto, RawProto)
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Permission (Perm (..))

-- import Wire.API.Team.Permission (Perm (..))

instance AsHeaders '[ConvId] Conversation Conversation where
  toHeaders c = (I (qUnqualified (cnvQualifiedId c)) :* Nil, c)
  fromHeaders = snd

type ConversationResponse = ResponseForExistedCreated Conversation

type ConversationHeaders = '[DescHeader "Location" "Conversation ID" ConvId]

type ConversationVerb =
  MultiVerb
    'POST
    '[JSON]
    '[ WithHeaders
         ConversationHeaders
         Conversation
         (Respond 200 "Conversation existed" Conversation),
       WithHeaders
         ConversationHeaders
         Conversation
         (Respond 201 "Conversation created" Conversation)
     ]
    ConversationResponse

type ConvUpdateResponses = UpdateResponses "Conversation unchanged" "Conversation updated" Event

type RemoveFromConversationVerb =
  MultiVerb
    'DELETE
    '[JSON]
    '[ RespondEmpty 204 "No change",
       Respond 200 "Member removed" Event
     ]
    (Maybe Event)

type ServantAPI =
  ConversationAPI
    :<|> TeamConversationAPI
    :<|> MessagingAPI
    :<|> TeamAPI
    :<|> FeatureAPI

type ConversationAPI =
  Named
    "get-unqualified-conversation"
    ( Summary "Get a conversation by ID"
        :> ZLocalUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Conversation
    )
    :<|> Named
           "get-conversation"
           ( Summary "Get a conversation by ID"
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> Get '[Servant.JSON] Conversation
           )
    :<|> Named
           "get-conversation-roles"
           ( Summary "Get existing roles available for the given conversation"
               :> ZLocalUser
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "roles"
               :> Get '[Servant.JSON] ConversationRolesList
           )
    :<|> Named
           "list-conversation-ids-unqualified"
           ( Summary "[deprecated] Get all local conversation IDs."
               -- FUTUREWORK: add bounds to swagger schema for Range
               :> ZLocalUser
               :> "conversations"
               :> "ids"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Conversation ID to start from (exclusive)"
                    ]
                    "start"
                    ConvId
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum number of IDs to return"
                    ]
                    "size"
                    (Range 1 1000 Int32)
               :> Get '[Servant.JSON] (ConversationList ConvId)
           )
    :<|> Named
           "list-conversation-ids"
           ( Summary "Get all conversation IDs."
               :> Description
                    "The IDs returned by this endpoint are paginated. To\
                    \ get the first page, make a call with the `paging_state` field set to\
                    \ `null` (or omitted). Whenever the `has_more` field of the response is\
                    \ set to `true`, more results are available, and they can be obtained\
                    \ by calling the endpoint again, but this time passing the value of\
                    \ `paging_state` returned by the previous call. One can continue in\
                    \ this fashion until all results are returned, which is indicated by\
                    \ `has_more` being `false`. Note that `paging_state` should be\
                    \ considered an opaque token. It should not be inspected, or stored, or\
                    \ reused across multiple unrelated invokations of the endpoint."
               :> ZLocalUser
               :> "conversations"
               :> "list-ids"
               :> ReqBody '[Servant.JSON] GetPaginatedConversationIds
               :> Post '[Servant.JSON] ConvIdsPage
           )
    :<|> Named
           "get-conversations"
           ( Summary "Get all *local* conversations."
               :> Description
                    "Will not return remote conversations.\n\n\
                    \Use `POST /conversations/list-ids` followed by \
                    \`POST /conversations/list/v2` instead."
               :> ZLocalUser
               :> "conversations"
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Mutually exclusive with 'start' (at most 32 IDs per request)"
                    ]
                    "ids"
                    (Range 1 32 (CommaSeparatedList ConvId))
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Conversation ID to start from (exclusive)"
                    ]
                    "start"
                    ConvId
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Maximum number of conversations to return"
                    ]
                    "size"
                    (Range 1 500 Int32)
               :> Get '[Servant.JSON] (ConversationList Conversation)
           )
    :<|> Named
           "list-conversations"
           ( Summary "Get conversation metadata for a list of conversation ids"
               :> ZLocalUser
               :> "conversations"
               :> "list"
               :> "v2"
               :> ReqBody '[Servant.JSON] ListConversations
               :> Post '[Servant.JSON] ConversationsResponse
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    :<|> Named
           "get-conversation-by-reusable-code"
           ( Summary "Get limited conversation information by key/code pair"
               :> CanThrow NotATeamMember
               :> CanThrow CodeNotFound
               :> CanThrow ConvNotFound
               :> CanThrow ConvAccessDenied
               :> CanThrow GuestLinksDisabled
               :> ZLocalUser
               :> "conversations"
               :> "join"
               :> QueryParam' [Required, Strict] "key" Code.Key
               :> QueryParam' [Required, Strict] "code" Code.Value
               :> Get '[Servant.JSON] ConversationCoverView
           )
    :<|> Named
           "create-group-conversation"
           ( Summary "Create a new conversation"
               :> CanThrow NotConnected
               :> CanThrow OperationDenied
               :> CanThrow NotATeamMember
               :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> ReqBody '[Servant.JSON] NewConvUnmanaged
               :> ConversationVerb
           )
    :<|> Named
           "create-self-conversation"
           ( Summary "Create a self-conversation"
               :> ZLocalUser
               :> "conversations"
               :> "self"
               :> ConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- TODO: add note: "On 201, the conversation ID is the `Location` header"
    :<|> Named
           "create-one-to-one-conversation"
           ( Summary "Create a 1:1 conversation"
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> "one2one"
               :> ReqBody '[Servant.JSON] NewConvUnmanaged
               :> ConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to members
    :<|> Named
           "add-members-to-conversation-unqualified"
           ( Summary "Add members to an existing conversation (deprecated)"
               :> CanThrow ConvNotFound
               :> CanThrow NotConnected
               :> CanThrow ConvAccessDenied
               :> CanThrow (InvalidOp "Invalid operation")
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "members"
               :> ReqBody '[JSON] Invite
               :> MultiVerb 'POST '[JSON] ConvUpdateResponses (UpdateResult Event)
           )
    :<|> Named
           "add-members-to-conversation"
           ( Summary "Add qualified members to an existing conversation."
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "members"
               :> "v2"
               :> ReqBody '[Servant.JSON] InviteQualified
               :> MultiVerb 'POST '[Servant.JSON] ConvUpdateResponses (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members
    :<|> Named
           "remove-member-unqualified"
           ( Summary "Remove a member from a conversation (deprecated)"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> Capture' '[Description "Target User ID"] "usr" UserId
               :> RemoveFromConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members
    :<|> Named
           "remove-member"
           ( Summary "Remove a member from a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> QualifiedCapture' '[Description "Target User ID"] "usr" UserId
               :> RemoveFromConversationVerb
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberStateUpdate event to members
    :<|> Named
           "update-other-member-unqualified"
           ( Summary "Update membership of the specified user (deprecated)"
               :> Description "Use `PUT /conversations/:cnv_domain/:cnv/members/:usr_domain/:usr` instead"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvNotFound
               :> CanThrow ConvMemberNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> Capture' '[Description "Target User ID"] "usr" UserId
               :> ReqBody '[JSON] OtherMemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Membership updated"]
                    ()
           )
    :<|> Named
           "update-other-member"
           ( Summary "Update membership of the specified user"
               :> Description "**Note**: at least one field has to be provided."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvNotFound
               :> CanThrow ConvMemberNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "members"
               :> QualifiedCapture' '[Description "Target User ID"] "usr" UserId
               :> ReqBody '[JSON] OtherMemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Membership updated"]
                    ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvRename event to members
    :<|> Named
           "update-conversation-name-deprecated"
           ( Summary "Update conversation name (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/name` instead."
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    [ ConvNotFound,
                      Respond 200 "Conversation updated" Event
                    ]
                    (Maybe Event)
           )
    :<|> Named
           "update-conversation-name-unqualified"
           ( Summary "Update conversation name (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/name` instead."
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "name"
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    [ ConvNotFound,
                      Respond 200 "Conversation updated" Event
                    ]
                    (Maybe Event)
           )
    :<|> Named
           "update-conversation-name"
           ( Summary "Update conversation name"
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "name"
               :> ReqBody '[JSON] ConversationRename
               :> MultiVerb
                    'PUT
                    '[JSON]
                    [ ConvNotFound,
                      Respond 200 "Conversation updated" Event
                    ]
                    (Maybe Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvMessageTimerUpdate event to members
    :<|> Named
           "update-conversation-message-timer-unqualified"
           ( Summary "Update the message timer for a conversation (deprecated)"
               :> Description "Use `/conversations/:domain/:cnv/message-timer` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "message-timer"
               :> ReqBody '[JSON] ConversationMessageTimerUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Message timer unchanged" "Message timer updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-message-timer"
           ( Summary "Update the message timer for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "message-timer"
               :> ReqBody '[JSON] ConversationMessageTimerUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Message timer unchanged" "Message timer updated" Event)
                    (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvReceiptModeUpdate event to members
    :<|> Named
           "update-conversation-receipt-mode-unqualified"
           ( Summary "Update receipt mode for a conversation (deprecated)"
               :> Description "Use `PUT /conversations/:domain/:cnv/receipt-mode` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "receipt-mode"
               :> ReqBody '[JSON] ConversationReceiptModeUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Receipt mode unchanged" "Receipt mode updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-receipt-mode"
           ( Summary "Update receipt mode for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "receipt-mode"
               :> ReqBody '[JSON] ConversationReceiptModeUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Receipt mode unchanged" "Receipt mode updated" Event)
                    (UpdateResult Event)
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members, if members get removed
    -- - ConvAccessUpdate event to members
    :<|> Named
           "update-conversation-access-unqualified"
           ( Summary "Update access modes for a conversation (deprecated)"
               :> Description "Use PUT `/conversations/:domain/:cnv/access` instead."
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "access"
               :> ReqBody '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "update-conversation-access"
           ( Summary "Update access modes for a conversation"
               :> ZLocalUser
               :> ZConn
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvNotFound
               :> CanThrow (InvalidOp "Invalid operation")
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "access"
               :> ReqBody '[JSON] ConversationAccessData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    (UpdateResponses "Access unchanged" "Access updated" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "get-conversation-self-unqualified"
           ( Summary "Get self membership properties (deprecated)"
               :> ZLocalUser
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> Get '[JSON] (Maybe Member)
           )
    :<|> Named
           "update-conversation-self-unqualified"
           ( Summary "Update self membership properties (deprecated)"
               :> Description "Use `/conversations/:domain/:conv/self` instead."
               :> CanThrow ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> Capture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> ReqBody '[JSON] MemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Update successful"]
                    ()
           )
    :<|> Named
           "update-conversation-self"
           ( Summary "Update self membership properties"
               :> Description "**Note**: at least one field has to be provided."
               :> CanThrow ConvNotFound
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture' '[Description "Conversation ID"] "cnv" ConvId
               :> "self"
               :> ReqBody '[JSON] MemberUpdate
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Update successful"]
                    ()
           )

type TeamConversationAPI =
  Named
    "get-team-conversation-roles"
    ( Summary "Get existing roles available for the given team"
        :> CanThrow NotATeamMember
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] ConversationRolesList
    )
    :<|> Named
           "get-team-conversations"
           ( Summary "Get team conversations"
               :> CanThrow OperationDenied
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Get '[Servant.JSON] TeamConversationList
           )
    :<|> Named
           "get-team-conversation"
           ( Summary "Get one team conversation"
               :> CanThrow OperationDenied
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> Get '[Servant.JSON] TeamConversation
           )
    :<|> Named
           "delete-team-conversation"
           ( Summary "Remove a team conversation"
               :> CanThrow NotATeamMember
               :> CanThrow ActionDenied
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "conversations"
               :> Capture "cid" ConvId
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Conversation deleted"] ()
           )

type TeamAPI =
  Named
    "create-non-binding-team"
    ( Summary "Create a new non binding team"
        :> ZUser
        :> ZConn
        :> CanThrow NotConnected
        :> "teams"
        :> ReqBody '[Servant.JSON] NonBindingNewTeam
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  '[DescHeader "Location" "Team ID" TeamId]
                  TeamId
                  (RespondEmpty 201 "Team ID as `Location` header value")
              ]
             TeamId
    )
    :<|> Named
           "update-team"
           ( Summary "Update team properties"
               :> ZUser
               :> ZConn
               :> CanThrow NotATeamMember
               :> CanThrow (OperationDeniedError 'SetTeamData)
               :> "teams"
               :> Capture "tid" TeamId
               :> ReqBody '[JSON] TeamUpdateData
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[RespondEmpty 200 "Team updated"]
                    ()
           )
    :<|> Named
           "get-teams"
           ( Summary "Get teams (deprecated); use `GET /teams/:tid`"
               :> ZUser
               :> "teams"
               :> Get '[JSON] TeamList
           )

type MessagingAPI =
  Named
    "post-otr-message-unqualified"
    ( Summary "Post an encrypted message to a conversation (accepts JSON or Protobuf)"
        :> Description PostOtrDescriptionUnqualified
        :> ZLocalUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> "otr"
        :> "messages"
        :> ReqBody '[Servant.JSON, Proto] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (Either (MessageNotSent ClientMismatch) ClientMismatch)
    )
    :<|> Named
           "post-proteus-message"
           ( Summary "Post an encrypted message to a conversation (accepts only Protobuf)"
               :> Description PostOtrDescription
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "proteus"
               :> "messages"
               :> ReqBody '[Proto] (RawProto QualifiedNewOtrMessage)
               :> MultiVerb
                    'POST
                    '[Servant.JSON]
                    (PostOtrResponses MessageSendingStatus)
                    (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
           )

type FeatureAPI =
  FeatureStatusGet 'TeamFeatureSSO
    :<|> FeatureStatusGet 'TeamFeatureLegalHold
    :<|> FeatureStatusPut 'TeamFeatureLegalHold
    :<|> FeatureStatusGet 'TeamFeatureSearchVisibility
    :<|> FeatureStatusPut 'TeamFeatureSearchVisibility
    :<|> FeatureStatusDeprecatedGet 'WithoutLockStatus 'TeamFeatureSearchVisibility
    :<|> FeatureStatusDeprecatedPut 'TeamFeatureSearchVisibility
    :<|> FeatureStatusGet 'TeamFeatureValidateSAMLEmails
    :<|> FeatureStatusDeprecatedGet 'WithoutLockStatus 'TeamFeatureValidateSAMLEmails
    :<|> FeatureStatusGet 'TeamFeatureDigitalSignatures
    :<|> FeatureStatusDeprecatedGet 'WithoutLockStatus 'TeamFeatureDigitalSignatures
    :<|> FeatureStatusGet 'TeamFeatureAppLock
    :<|> FeatureStatusPut 'TeamFeatureAppLock
    :<|> FeatureStatusGet 'TeamFeatureFileSharing
    :<|> FeatureStatusPut 'TeamFeatureFileSharing
    :<|> FeatureStatusGet 'TeamFeatureClassifiedDomains
    :<|> FeatureStatusGet 'TeamFeatureConferenceCalling
    :<|> FeatureStatusGet 'TeamFeatureSelfDeletingMessages
    :<|> FeatureStatusPut 'TeamFeatureSelfDeletingMessages
    :<|> FeatureStatusGet 'TeamFeatureGuestLinks
    :<|> FeatureStatusPut 'TeamFeatureGuestLinks
    :<|> AllFeatureConfigsGet
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureLegalHold
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureSSO
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureSearchVisibility
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureValidateSAMLEmails
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureDigitalSignatures
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureAppLock
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureFileSharing
    :<|> FeatureConfigGet 'WithoutLockStatus 'TeamFeatureClassifiedDomains
    :<|> FeatureConfigGet 'WithLockStatus 'TeamFeatureConferenceCalling
    :<|> FeatureConfigGet 'WithLockStatus 'TeamFeatureSelfDeletingMessages
    :<|> FeatureConfigGet 'WithLockStatus 'TeamFeatureGuestLinks

type FeatureStatusGet featureName =
  Named
    '("get", featureName)
    ( Summary (AppendSymbol "Get config for " (KnownTeamFeatureNameSymbol featureName))
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> KnownTeamFeatureNameSymbol featureName
        :> Get '[Servant.JSON] (TeamFeatureStatus 'WithLockStatus featureName)
    )

type FeatureStatusPut featureName =
  Named
    '("put", featureName)
    ( Summary (AppendSymbol "Put config for " (KnownTeamFeatureNameSymbol featureName))
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> KnownTeamFeatureNameSymbol featureName
        :> ReqBody '[Servant.JSON] (TeamFeatureStatus 'WithoutLockStatus featureName)
        :> Put '[Servant.JSON] (TeamFeatureStatus 'WithoutLockStatus featureName)
    )

-- | A type for a GET endpoint for a feature with a deprecated path
type FeatureStatusDeprecatedGet ps featureName =
  Named
    '("get-deprecated", featureName)
    ( Summary
        (AppendSymbol "[deprecated] Get config for " (KnownTeamFeatureNameSymbol featureName))
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> DeprecatedFeatureName featureName
        :> Get '[Servant.JSON] (TeamFeatureStatus ps featureName)
    )

-- | A type for a PUT endpoint for a feature with a deprecated path
type FeatureStatusDeprecatedPut featureName =
  Named
    '("put-deprecated", featureName)
    ( Summary
        (AppendSymbol "[deprecated] Get config for " (KnownTeamFeatureNameSymbol featureName))
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> DeprecatedFeatureName featureName
        :> ReqBody '[Servant.JSON] (TeamFeatureStatus 'WithoutLockStatus featureName)
        :> Put '[Servant.JSON] (TeamFeatureStatus 'WithoutLockStatus featureName)
    )

type FeatureConfigGet ps featureName =
  Named
    '("get-config", featureName)
    ( Summary (AppendSymbol "Get feature config for feature " (KnownTeamFeatureNameSymbol featureName))
        :> ZUser
        :> "feature-configs"
        :> KnownTeamFeatureNameSymbol featureName
        :> Get '[Servant.JSON] (TeamFeatureStatus ps featureName)
    )

type AllFeatureConfigsGet =
  Named
    "get-all-feature-configs"
    ( Summary "Get configurations of all features"
        :> ZUser
        :> "feature-configs"
        :> Get '[Servant.JSON] AllFeatureConfigs
    )

type PostOtrDescriptionUnqualified =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts two query params:\n\
  \- `ignore_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are ignored.\n\
  \  - When 'false' all missing clients are reported.\n\
  \  - When comma separated list of user-ids, only clients for listed users are ignored.\n\
  \- `report_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are reported.\n\
  \  - When 'false' all missing clients are ignored.\n\
  \  - When comma separated list of user-ids, only clients for listed users are reported.\n\
  \\n\
  \Apart from these, the request body also accepts `report_missing` which can only be a list of user ids and behaves the same way as the query parameter.\n\
  \\n\
  \All three of these should be considered mutually exclusive. The server however does not error if more than one is specified, it reads them in this order of precedence:\n\
  \- `report_missing` in the request body has highest precedence.\n\
  \- `ignore_missing` in the query param is the next.\n\
  \- `report_missing` in the query param has the lowest precedence.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

type PostOtrDescription =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts `client_mismatch_strategy` in the body. It can have these values:\n\
  \- `report_all`: When set, the message is not sent if any clients are missing. The missing clients are reported in the response.\n\
  \- `ignore_all`: When set, no checks about missing clients are carried out.\n\
  \- `report_only`: Takes a list of qualified UserIDs. If any clients of the listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \- `ignore_only`: Takes a list of qualified UserIDs. If any clients of the non-listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \\n\
  \The sending of messages in a federated conversation could theorectically fail partially. \
  \To make this case unlikely, the backend first gets a list of clients from all the involved backends and then tries to send a message. \
  \So, if any backend is down, the message is not propagated to anyone. \
  \But the actual message fan out to multiple backends could still fail partially. This type of failure is reported as a 201, \
  \the clients for which the message sending failed are part of the response body.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
