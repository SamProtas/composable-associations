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

      module Data.ComposableAssociation

      -- * Invalid JSON Encoding Exception
    , JsonObjectEncodingException (..)
    ) where


import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Control.Exception

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.HashMap.Lazy as HashMap

import Data.ComposableAssociation

--      -- * Re-Exported Types
--      Association (..)
--    , (:<>) (..)
--    , WithAssociation
--
--    -- * Re-Exported Helpers
--    , withAssociation
--    , asValue
--    , reKey
--
--    -- * Re-Exported Lens
--    , _value
--    , _keyValue
--    , _base
--
--
--

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
  parseJSON = withObject "Association" $ \v -> Association proxy <$> (v .: key >>= parseJSON)
    where proxy = Proxy :: Proxy key
          key = T.pack $ symbolVal proxy :: Text

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
                                            fmap (Association proxy) (v' .: key >>= parseJSON)
    where proxy = Proxy :: Proxy key
          key = T.pack $ symbolVal proxy


-- $quickstart
-- Assume some example data below:
--
-- >>> data ExampleUser = ExampleUser { name :: String, age :: Int } deriving (Show, Eq, Generic)
-- >>> instance ToJSON ExampleUser
-- >>> instance FromJSON ExampleUser
--
-- >>> let alice = ExampleUser { name = "Alice", age = 25 }
-- >>> encode alice
-- "{\"age\":25,\"name\":\"Alice\"}"
--
-- >>> let messageIds = [102, 305, 410]
-- >>> encode messageIds
-- "[102,305,410]"
--
-- Let's add those messages to the user JSON object without bothering to define another type.
--
-- >>> let aliceWithMessages = alice :<> (asValue messageIds :: Association "messages" [Int])
-- >>> encode aliceWithMessages
-- "{\"age\":25,\"name\":\"Alice\",\"messages\":[102,305,410]}"
--
-- Since "messages" is type (not value) information, we can decode as well.
--
-- >>> decode "{\"age\":25,\"name\":\"Alice\",\"messages\":[102,305,410]}" :: Maybe (ExampleUser :<> Association "messages" [Int])
-- Just (ExampleUser {name = "Alice", age = 25} :<> Association Proxy [102,305,410]) -- "Proxy" is the value of type "messages"
--
-- @Association@ @Proxy@ @a@ has a stand-alone encoding/decoding too
--
-- >>> encode $ Association (Proxy :: Proxy "one-off-key") [1, 2, 3]
-- "{\"one-off-key\":[1,2,3]}"
--
-- >>> decode "{\"one-off-key\":[1,2,3]}" :: Maybe (Association "one-off-key" [Int])
-- Just (Association Proxy [1,2,3])
--
-- These are chainable too!
--
-- >>> encode (alice :<> asValue [1,2,3] :<> asValue [True, False] :: ExampleUser :<> Association "numbers" [Int] :<> Association "bools" [Bool])
-- "{\"age\":25,\"name\":\"Alice\",\"bools\":[true,false],\"numbers\":[1,2,3]}"
--
-- You can build JSON objects from just values!
--
-- >>> encode (asValue True :<> asValue "Hello" :<> asValue alice :: Association "a-bool" Bool :<> Association "a-string" String :<> Association "an-alice" ExampleUser)
-- "{\"a-bool\":true,\"an-alice\":{\"age\":25,\"name\":\"Alice\"},\"a-string\":\"Hello\"}"
--
-- Decoding fails if you specify a non-existent key (standard Aeson behavior for failed decoding).
--
-- >>> decode "{\"one-off-key\":[1,2,3]}" :: Maybe (Association "wrong-key" [Int])
-- Nothing
--
-- If you try encoding with a "base" value that is itself not encoded to a JSON object you'll get a runtime exception.
--
-- >>> encode $ True :<> (asValue [1,2,3] :: Association "this-ends-poorly" [Int])
-- *** Exception: NoObjectRepException (Bool True)
-- >>> encode $ [1,2,3] :<> (asValue "will not work" :: Association "still" String)
-- *** Exception: NoObjectRepException (Array [Number 1.0,Number 2.0,Number 3.0])
--
-- Note:
-- You'll need @DataKinds@ for this library.
-- You'll probably want @TypeOperators@ as well.
-- You can avoid






