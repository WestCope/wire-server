{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Federator.Federate where

import Data.Domain (Domain (Domain))
import Federator.Federate
import Imports
import Mu.GRpc.Client.Record
import qualified Network.HTTP.Types as HTTP
import Network.HTTP2.Client (TooMuchConcurrency (TooMuchConcurrency))
import Polysemy (embed, runM)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

genMock ''Remote
genMock ''Brig

tests :: TestTree
tests =
  testGroup "Fedderate" $
    [ testGroup "with remote" $
        [ remoteCallSuccess,
          remoteCallFailureTMC,
          remoteCallFailureErrCode,
          remoteCallFailureErrStr
        ],
      testGroup "with local" $
        [ localCallBrigSuccess
        ]
    ]

remoteCallSuccess :: TestTree
remoteCallSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (GRpcOk (ResponseOk "success!")))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) "GET" "/users?handle=foo" mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertEqual "successful response should be returned" (ResponseOk "success!") res

-- FUTUREWORK: This is probably not ideal, we should figure out what this error
-- means and act accordingly.
remoteCallFailureTMC :: TestTree
remoteCallFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (GRpcTooMuchConcurrency (TooMuchConcurrency 2)))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) "GET" "/users?handle=foo" mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrCode :: TestTree
remoteCallFailureErrCode =
  testCase "should respond with error when facing GRpcErrorCode" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (GRpcErrorCode 77)) -- TODO: Maybe use some legit HTTP2 error code?
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) "GET" "/users?handle=foo" mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrStr :: TestTree
remoteCallFailureErrStr =
  testCase "should respond with error when facing GRpcErrorString" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (GRpcErrorString "some grpc error")) -- Maybe use some legit HTTP2 error code?
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) "GET" "/users?handle=foo" mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

localCallBrigSuccess :: TestTree
localCallBrigSuccess =
  testCase "should sucessfully return on HTTP 200" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ -> pure (HTTP.status200, "response body"))
      let localCall = LocalCall (Just Brig) "GET" "/users?handle=foo" mempty
      res <- mock @Brig @IO $ callLocal localCall
      actualCalls <- mockBrigCallCalls @IO
      embed $ assertEqual "one call to brig should be made" [("GET", "/users?handle=foo", mempty)] actualCalls
      embed $ assertEqual "response should be success with correct body" (ResponseOk "response body") res

isResponseError :: Response -> Bool
isResponseError (ResponseErr _) = True
isResponseError (ResponseOk _) = False
