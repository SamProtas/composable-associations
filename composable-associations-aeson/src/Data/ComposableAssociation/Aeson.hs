{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.ComposableAssociation.Aeson
    ( -- * Quickstart
      -- $quickstart

      -- * Re-Exported Core Types\/Functions\/Lens
      module Data.ComposableAssociation

      -- * Invalid JSON Encoding Exception
    , JsonObjectEncodingException (..)
    ) where


import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Control.Exception

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HashMap

import Data.ComposableAssociation


-- | More specific version of @ObjectEncodingException@ to only Aeson encoding issues.
newtype JsonObjectEncodingException = JsonObjectEncodingException Value deriving (Show, Typeable)
instance Exception JsonObjectEncodingException where
  toException = toException . ObjectEncodingException
  fromException x = do
    ObjectEncodingException e <- fromException x
    cast e



instance (ToJSON obj, KnownSymbol key) => ToJSON (Association key obj) where
  toJSON (Association key obj) = object [keyName .= toJSON obj]
    where keyName = T.pack $ symbolVal key

instance (FromJSON obj, KnownSymbol key) => FromJSON (Association key obj) where
  parseJSON = withObject "Association" $ \v' -> Association proxy <$> (v' .:? key .!= Null >>= parseJSON)
      where proxy = Proxy :: Proxy key
            key = T.pack $ symbolVal proxy


-- | Throws a @JsonObjectEncodingException@ if the base value isn't encoded as a JSON object
instance (ToJSON base, ToJSON obj, KnownSymbol key) => ToJSON (base :<> Association key obj) where
  toJSON (base :<> Association key obj) = Object $ HashMap.insert keyName objJson baseJsonMap
    where keyName = T.pack $ symbolVal key
          baseJsonMap = case toJSON base of (Object jsonObjVal) -> jsonObjVal
                                            notAnObject -> throw $ JsonObjectEncodingException notAnObject
          objJson = toJSON obj

instance (FromJSON base, FromJSON obj, KnownSymbol key) => FromJSON (base :<> Association key obj) where
  parseJSON = withObject "base :<> assoc" $ \v' -> (:<>) <$>
                                            parseJSON (Object $ HashMap.delete key v') <*>
                                            fmap (Association proxy) (v' .:? key .!= Null >>= parseJSON)
    where proxy = Proxy :: Proxy key
          key = T.pack $ symbolVal proxy

-- $setup
-- >>> import GHC.Generics

-- $quickstart
-- Assume some example data below:
--
-- >>> data ExampleUser = ExampleUser { name :: String, age :: Int } deriving (Show, Eq, Generic)
-- >>> instance ToJSON ExampleUser
-- >>> instance FromJSON ExampleUser
-- >>> data ExampleUserWithMessages = ExampleUserWithMessages { name :: String, age :: Int, messages :: [Int] } deriving (Show, Eq, Generic)
-- >>> instance ToJSON ExampleUserWithMessages
-- >>> instance FromJSON ExampleUserWithMessages
--
-- >>> let aliceName = "Alice"
-- >>> let aliceAge = 25
-- >>> let messageIds = [102, 305, 410]
-- >>> let alice = ExampleUser aliceName aliceAge
-- >>> let aliceWithMessages = ExampleUserWithMessages aliceName aliceAge messageIds
--
--
-- Let's add those messages to the alice object without requiring our custom "WithMessages" version of the User type.
--
-- >>> let adHocAliceWithMessages = alice :<> (asValue messageIds :: Association "messages" [Int])
-- >>> encode aliceWithMessages == encode adHocAliceWithMessages
-- True
--
-- Since "messages" is type (not value) information, we can decode as well.
--
-- >>> decode "{\"age\":25,\"name\":\"Alice\",\"messages\":[102,305,410]}" :: Maybe (ExampleUser :<> Association "messages" [Int])
-- Just (ExampleUser {name = "Alice", age = 25} :<> Association Proxy [102,305,410])
--
-- In the above, "Proxy" is the value of type "messages".
--
-- @Association Proxy a@ has a stand-alone encoding/decoding too
--
-- >>> encode $ Association (Proxy :: Proxy "one-off-key") [1, 2, 3]
-- "{\"one-off-key\":[1,2,3]}"
--
-- >>> decode "{\"one-off-key\":[1,2,3]}" :: Maybe (Association "one-off-key" [Int])
-- Just (Association Proxy [1,2,3])
--
-- You can build JSON objects from just values!
--
-- >>> :{
-- let allValues :: Association "name" String :<> Association "age" Int
--     allValues = asValue aliceName :<> asValue aliceAge
-- in encode allValues == encode alice
-- :}
-- True
--
-- Decoding fails if you specify a non-existent key (standard Aeson behavior for failed decoding).
--
-- >>> decode "{\"one-off-key\":[1,2,3]}" :: Maybe (Association "wrong-key" [Int])
-- Nothing
--
-- If you try encoding with a "base" value that is itself not encoded to a JSON object you'll get a runtime exception.
--
-- >>> encode $ True :<> (asValue [1,2,3] :: Association "this-ends-poorly" [Int])
-- "*** Exception: JsonObjectEncodingException (Bool True)
--
-- GHC Extension Note:
--
-- * You'll need @DataKinds@ for this library (type level literals, no getting around this).
--
-- * You'll probably want @TypeOperators@ as well (although you can use @WithAssociation@ instead of @:<>@ to avoid this).
--
-- * You can avoid @PolyKinds@ if you use @asValue True :: Association "key" Bool@ or type inference instead of
-- @Association (Proxy :: Proxy "key") True@.






