{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

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

module Test.Federator.Util where

import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Polysemy
import Polysemy.Error
import Test.Tasty.HUnit
import Wire.API.Federation.Domain

assertNoError :: (Show e, Member (Embed IO) r) => Sem (Error e ': r) x -> Sem r x
assertNoError =
  runError >=> \case
    Left err -> embed @IO . assertFailure $ "Unexpected error: " <> show err
    Right x -> pure x

data TestRequest = TestRequest
  { trCertificateHeader :: Maybe ByteString,
    trDomainHeader :: Maybe ByteString,
    trPath :: ByteString,
    trBody :: LByteString,
    trMethod :: HTTP.Method,
    trExtraHeaders :: [HTTP.Header]
  }

instance Default TestRequest where
  def =
    TestRequest
      { trCertificateHeader = Nothing,
        trDomainHeader = Nothing,
        trPath = mempty,
        trBody = mempty,
        trMethod = HTTP.methodPost,
        trExtraHeaders = [("Content-Type", "application/json")]
      }

testRequest :: TestRequest -> IO Wai.Request
testRequest tr = do
  refChunks <- liftIO $ newIORef $ LBS.toChunks (trBody tr)
  pure . flip Wai.setPath (trPath tr) $
    Wai.defaultRequest
      { Wai.requestMethod = trMethod tr,
        Wai.requestBody = atomicModifyIORef refChunks $ \case
          [] -> ([], mempty)
          x : y -> (y, x),
        Wai.requestHeaders =
          [("X-SSL-Certificate", HTTP.urlEncode True h) | h <- toList (trCertificateHeader tr)]
            <> [(originDomainHeaderName, h) | h <- toList (trDomainHeader tr)]
            <> trExtraHeaders tr
      }
