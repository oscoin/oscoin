-- | Tests behavior of the Node through its API using a test
-- implementation of 'MonadClient'.
module Oscoin.Test.API
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import qualified Radicle.Extended as Rad

import qualified Data.Text as T

import qualified Oscoin.Test.API.HTTP.TestClient as Client
import           Oscoin.Test.HTTP.Helpers
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck


tests :: [TestTree]
tests =
    [ testGroup "getState"
        [ testCase "existing value" $ do
            path <- generate $ listOf $ arbitraryRadicleIdent
            -- TODO Generate arbitrary Radicle values once #258 is fixed
            let radValue = Rad.String "hooray!"
            let env = initRadicleEnv [(T.intercalate "/" path, radValue)]
            runSession (nodeState mempty (blockchainFromEnv env)) $ do
                result <- Client.run (Client.getState path)
                result @?= API.Ok radValue

        , testCase "non-existing value" $ do
            path <- generate $ listOf $ arbitraryRadicleIdent
            let env = initRadicleEnv []
            runSession (nodeState mempty (blockchainFromEnv env)) $ do
                result <- Client.run (Client.getState path)
                result @?= API.Err "Value not found"

        , testCase "existing reference" $ do
            refName <- generate $ arbitraryRadicleIdent
            -- TODO Generate arbitrary Radicle values once #258 is fixed
            let radValue = Rad.String "hooray!"
            let env = initRadicleEnv []
                      & addRadicleRef refName radValue
            runSession (nodeState mempty (blockchainFromEnv env)) $ do
                result <- Client.run (Client.getState [refName])
                result @?= API.Ok radValue
        ]
    ]


arbitraryRadicleIdent :: Gen Text
arbitraryRadicleIdent = T.pack <$> listOf1 (elements ['a'..'z'])
