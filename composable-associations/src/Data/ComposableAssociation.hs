{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.ComposableAssociation
    ( -- * Description
      -- $header

      -- * Base Types
      Association (..)
    , (:<>) (..)
    , WithAssociation

      -- * Helper Functions
    , withAssociation
    , asValue
    , reKey

      -- * Lens
    , _value
    , _keyValue
    , _base

      -- * Generic Invalid Encoding Exception
    , ObjectEncodingException (..)
    ) where

import GHC.Generics
import Data.Proxy
import Control.Exception
import Data.Typeable

-- | A type representing a key-value association where the "key" itself exists only at the type level.
--
-- >>> let x = Association Proxy [1, 2, 3] :: Asssociation "type_level_key" [Int]
--
-- This type exists primarily as a way to "tag" data with a key for the purpose of serializing haskell data into
-- formats that have a key-value representation (ex: a JSON object).
--
-- The example above represents a serializable key-value pair with a key of "type_level_key" and a value of [1, 2, 3].
--
-- Because the key itself is represented at the type-level, this single data constructor can easily represent wrapping
-- any haskell value in a key-value association for serialization/deserialization.
data Association key value = Association (Proxy key) value
                      deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | A type representing the composition of a base type (which can be serialized into a key-value structure) along with a key-value type.
--
-- This type exists as a way to compose a haskell value that has a key-value representation (ex: a haskell
-- record where its fields are keys to their values) with additional key-value associations into a single key-value
-- object.
--
-- This is intended for use with @Association@ to add additional key-values to a type for the purposes of
-- serialization/deserialization.
--
-- For example:
--
-- >>> data User = User { name :: String, age :: Int }
-- >>> let alice = User "Alice" 26
-- >>> let bob = User "Bob" 25
-- >>> let charlie = User "Charlie" 27
-- >>> let bobsFriends = [alice, charlie]
-- >>> bobAndFriends :: User :<> Association "friends" [User]
-- >>> let bobAndFriends = bob :<> Association Proxy bobsFriends
--
-- While @(bob, bobsFriends)@ contains the same values as @bobAndFriends@, it lacks information about how to add
-- @bobsFriends@ to to combine this information into a single serialized key-value object (as well as how to deserialize
-- it).
data base :<> assoc = base :<> assoc
                       deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Type alias for the (:<>) type operator
--
-- Useful if you don't like the TypeOperators extension
type WithAssociation base assoc = base :<> assoc

-- | Function alias for the (:<>) type constructor
withAssociation :: a -> b -> WithAssociation a b
withAssociation = (:<>)

-- | @_value :: Lens value value' (SetKey key value) (SetKey key value')@
_value :: Functor f => (value -> f value') -> Association key value -> f (Association key value')
_value inj (Association key value) = Association key <$> inj value

-- | @_keyValue :: Lens keyValue keyValue' (base :<> keyValue) (base :<> keyValue')@
_keyValue :: Functor f => (keyValue -> f keyValue') -> base :<> keyValue -> f (base :<> keyValue')
_keyValue inj (base :<> keyValue) = (:<>) base <$> inj keyValue

-- | @_base :: Lens base base' (base :<> keyValue) (base' :<> keyValue)@
_base :: Functor f => (base -> f base') -> base :<> keyValue -> f (base' :<> keyValue)
_base inj (base :<> keyValue) =  flip (:<>) keyValue <$> inj base

-- | Convenience function for creating associations.
--
-- This is especially useful when type-inference elsewhere in your program will determine the type of the Association.
--
-- >>> let x = asValue True :: Association "whatever_key" Bool
asValue :: obj -> Association key obj
asValue = Association Proxy

-- | Convenience function for changing the type of the "key"
--
-- >>> let x = Association Proxy 10 :: Association "key_x" Int
-- >>> let y = reKey x :: Association "key_y" Int
reKey :: Association key obj -> Association key' obj
reKey (Association _ obj) = Association Proxy obj

-- | Generic encoding exception for when a @:<>@ "base" cannot be encoded as something object-like.
--
-- Each serialization should have a more specific version of this exception to convey information about the failure.
data ObjectEncodingException = forall e. Exception e => ObjectEncodingException e deriving Typeable

instance Show ObjectEncodingException where
  show (ObjectEncodingException e) = show e

instance Exception ObjectEncodingException

-- $header
-- This library exports basic types, helper functions, and hand-written Lens's .
--
-- Unless you're implementing a serialization format library (orphan instances for these types to implement
-- serialization/deserialization for some format) you probably are in the wrong place. Please see other packages under
-- this namespace for useful typeclass implementations, they should re-export this module.