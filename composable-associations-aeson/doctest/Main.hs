{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-XOverloadedStrings", "-XDeriveGeneric", "-XDataKinds", "-XTypeOperators",
                 "src/Data/ComposableAssociation/Aeson.hs" ]