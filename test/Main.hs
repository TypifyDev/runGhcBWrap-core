{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import qualified TypeSigSpec
import qualified HKTsSpec
import qualified FFISpec

main :: IO ()
main = hspec $ do
  TypeSigSpec.spec
  HKTsSpec.spec
  FFISpec.spec
