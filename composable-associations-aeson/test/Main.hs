{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics
import Data.Maybe
import Data.Either

import Data.Proxy
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Exception

import Data.ComposableAssociation.Aeson

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "Just (x :: Association \"Some Key\" TestUser) == decode . encode x" $ \user ->
      let userAsKey = asValue user :: Association "user" TestUser
      in Just userAsKey == (decode . encode) userAsKey
  , testProperty "Just (x :: TestUser :<> Association \"Some Key\" [Int]) == decode . encode x" $
      \(user :: TestUser, messageIds) ->
      let userWithMessages = user :<> (asValue messageIds :: Association "message_ids" [Int])
      in Just userWithMessages == (decode . encode) userWithMessages
  , testProperty "Association encode/decode order doesn't matter" $ \(ids, results) ->
      let idsAndMessages = asValue ids :<> asValue results :: Association "ids" [Int] :<> Association "results" [Bool]
      in isJust ((decode . encode) idsAndMessages :: Maybe (Association "results" [Bool] :<> Association "ids" [Int]))
  , testProperty "Nested Association work" $ \(user :: TestUser, messageIds, results) ->
      let userWithMessagesAndResults :: TestUser :<> Association "messages" [Int] :<> Association "results" [Bool]
          userWithMessagesAndResults = user :<> asValue messageIds :<> asValue results
      in Just userWithMessagesAndResults == (decode . encode) userWithMessagesAndResults ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Association ToJSON Instance" $ encode testUser1FriendsAssoc @?= "{\"message_ids\":[1,2,3]}"
  , testCase "Association FromJSON Instance" $ decode "{\"message_ids\":[1,2,3]}" @?= Just testUser1FriendsAssoc
  , testCase ":<> ToJSON" $
      encode (testUser1 :<> testUser1FriendsAssoc) @?= encode testUser1WithMessages
  , testCase ":<> FromJSON" $
      decode "{\"message_ids\":[1,2,3],\"userId\":1,\"name\":\"Sam\"}" @?= Just (testUser1 :<> testUser1FriendsAssoc)
  , testCase ":<> Invalid Encoding1" $ do
      res <- try (evaluate $ encode ([1 :: Int] :<> testUser1FriendsAssoc)) :: IO (Either JsonObjectEncodingException ByteString)
      assertBool "Non-Json-Obj base throws JsonObjectEncodingException" (isLeft res)
  , testCase ":<> Invalid Encoding2" $ do
      res <- try (evaluate $ encode ([1 :: Int] :<> testUser1FriendsAssoc)) :: IO (Either ObjectEncodingException ByteString)
      assertBool "Non-Json-Obj base throws ObjectEncodingException" (isLeft res)
  , testCase "Encode Association Null" $ encode testMissingMessages @?= "{\"message_ids\":null}"
  , testCase "Decode Association Null" $ decode "{\"message_ids\":null}" @?= Just testMissingMessages
  , testCase "Decode Association Missing Key" $ decode "{}" @?= Just testMissingMessages
  , testCase "Encode :<> Missing Association" $
      encode (testUser1 :<> testMissingMessages) @?= encode testUser1WithMessages { withmessages_message_ids = Nothing }
  , testCase "Decode :<> Missing Association" $
      decode "{\"message_ids\":null,\"userId\":1,\"name\":\"Sam\"}" @?= Just (testUser1 :<> testMissingMessages)
  , testCase "Decode :<> Missing Association Key" $
      decode "{\"userId\":1,\"name\":\"Sam\"}" @?= Just (testUser1 :<> testMissingMessages)]


-- Test Data:

data TestUser = TestUser { userId :: Int
                         , name :: String }
                         deriving (Show, Eq, Generic)
instance ToJSON TestUser
instance FromJSON TestUser

data TestUserWithMessages = TestUserWithMessages { withMessages_userId :: Int
                                                 , withMessages_name :: String 
                                                 , withmessages_message_ids :: Maybe [Int]}
                                                 deriving (Show, Eq, Generic)
instance ToJSON TestUserWithMessages where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = Prelude.drop 13 }
instance FromJSON TestUserWithMessages where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = Prelude.drop 13 }

testUser1 :: TestUser
testUser1 = TestUser { userId = 1, name = "Sam" }

testUser1WithMessages :: TestUserWithMessages
testUser1WithMessages = TestUserWithMessages 1 "Sam" (Just [1, 2, 3])

testUser1FriendsAssoc :: Association "message_ids" [Int]
testUser1FriendsAssoc = Association Proxy [1, 2, 3]

testAssoc :: TestUser :<> Association "message_ids" [Int] :<> Association "results" [Bool]
testAssoc = testUser1 :<> testUser1FriendsAssoc :<> Association Proxy [True, False, True]

testMissingMessages :: Association "message_ids" (Maybe [Int])
testMissingMessages = asValue Nothing


instance Arbitrary TestUser where
  arbitrary = TestUser <$> arbitrary <*> arbitrary
