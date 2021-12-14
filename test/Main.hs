{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Faro

prop_test :: Property
prop_test = property $ do
  doFaro === "Faro"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
