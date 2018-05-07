module Oscoin.Prototype.V2.Tests (tests) where

import           Oscoin.Prelude

import           Oscoin.Prototype.V2

import           Control.Concurrent
import           Data.IORef
import qualified Data.Map as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec (parseMaybe)

tests :: [TestTree]
tests = [evalSpec, parserSpec, runChainFromChannelSpec]

evalSpec :: TestTree
evalSpec = testGroup "eval"
    [ testCase "Fails for undefined atoms" $
        eval (Atom "blah") `failsWith` UnknownIdentifier "blah"

    , testCase "Succeeds for defined atoms" $ do
        let prog = do
              define "rocky-clark" (String "Steve Wozniak")
              eval (Atom "rocky-clark")
        prog `succeedsWith` (String "Steve Wozniak")

    , testCase "'sorted-map' creates a SortedMap with given key/vals" $ do
        let prog = eval (Primop "sorted-map" $$ [String "why", String "not"])
        prog `succeedsWith` (SortedMap $ Map.fromList [(String "why", String "not")])

    , testCase "'eval' evaluates the list" $ do
        let prog = eval (Primop "eval" $$ [List [Primop "sorted-map"]])
        prog `succeedsWith` (SortedMap $ Map.fromList [])

    , testCase "lambdas work" $ do
        let prog = eval (Lambda ["x"] [Atom "x"] mempty $$ [Boolean True])
        prog `succeedsWith` (Boolean True)

    , testCase "lambda does not have access to future definitions" $ do
        let prog = do
              define "y" (String "a")
              l <- lambda [] [Atom "y"]
              define "y" (String "b")
              eval $ l $$ []
        prog `succeedsWith` String "a"

    , testCase "'if' works with three arguments and true cond" $ do
        let prog = eval (If (Boolean True) (String "a") (String "b"))
        prog `succeedsWith` String "a"

    , testCase "'if' works with three arguments and false cond" $ do
        let prog = eval (If (Boolean False) (String "a") (String "b"))
        prog `succeedsWith` String "b"

    , testCase "'if' is lazy" $ do
        let prog = eval (If (Boolean True) (String "a") errValue)
        prog `succeedsWith` String "a"
    ]
  where
    errValue = Boolean True $$ [String "a"]
    failsWith val err = fst (runLangM mempty val) @?= Left err
    succeedsWith val val'  = fst (runLangM mempty val) @?= Right val'

parserSpec :: TestTree
parserSpec = testGroup "valueP parser" $
    [ testCase "parses strings" $ do
        "\"hi\"" ==> String "hi"

    , testCase "parses booleans" $ do
        "#t" ==> Boolean True
        "#f" ==> Boolean False

    , testCase "parses primops" $ do
        "sorted-map" ==> Primop "sorted-map"
        "boolean?" ==> Primop "boolean?"

    , testCase "parses identifiers" $ do
        "++" ==> Atom "++"
        "what?crazy!" ==> Atom "what?crazy!"

    , testCase "parses function application" $ do
        "(++)" ==> (Atom "++" $$ [])
        "(++ \"merge\" \"d\")" ==> (Atom "++" $$ [String "merge", String "d"])
    ]
  where
    x ==> y = parseMaybe valueP x @?= Just y

runChainFromChannelSpec :: TestTree
runChainFromChannelSpec = testGroup "runChainFromChannel"
    [ testCase "Runs until chainStep returns Nothing" $ do
        callsRef <- newIORef (0 :: Int)
        let def x = Define (Ident x) (String x)
            chain = genesisChain
                { chainStep = \e t -> if t == def "stop"
                    then Left (OtherError "stop!")
                    else Right (Atom "step", e)
                , chainCallback = \_ -> modifyIORef callsRef succ
                }
        chan <- runChain chain
        traverse (writeChan chan) [def "1", def "2", def "stop", def "4"]
        threadDelay 1000
        result <- readIORef callsRef
        result @?= 3
    ]
  where
    runChain chain = do
        chan <- newChan
        forkIO $ runChainFromChannel chan chain
        return chan
