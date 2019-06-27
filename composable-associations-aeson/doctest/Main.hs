{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-XOverloadedStrings", "-XDeriveGeneric", "-XDataKinds", "-XTypeOperators", "-isrc",
                 "src/Data/ComposableAssociation/Aeson.hs" ]
