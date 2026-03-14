{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeSigSpec where

import Test.Hspec hiding (Example)
import Data.Proxy
import Data.Typeable (typeRep)
import RunGhc.MakeTest.TypeSig

spec :: Spec
spec = describe "RunGhc.MakeTest.TypeSig" $ do

  ---------------------------------------------------------------------------
  -- A. slotUser / slotTest for every Slot variant
  ---------------------------------------------------------------------------
  describe "slotUser (single Poly slot)" $ do
    it "Poly 0 Int -> a" $
      slotUser (Proxy @(Poly 0 Int)) `shouldBe` "a"
    it "Poly 1 Bool -> b" $
      slotUser (Proxy @(Poly 1 Bool)) `shouldBe` "b"
    it "Poly 2 Char -> c" $
      slotUser (Proxy @(Poly 2 Char)) `shouldBe` "c"
    it "Poly 3 Double -> d" $
      slotUser (Proxy @(Poly 3 Double)) `shouldBe` "d"
    it "Poly 0 [Int] -> a (complex concrete type does not affect user view)" $
      slotUser (Proxy @(Poly 0 [Int])) `shouldBe` "a"
    it "Poly 0 (Maybe Int) -> a" $
      slotUser (Proxy @(Poly 0 (Maybe Int))) `shouldBe` "a"

  describe "slotTest (single Poly slot)" $ do
    it "Poly 0 Int -> Int" $
      slotTest (Proxy @(Poly 0 Int)) `shouldBe` "Int"
    it "Poly 1 Bool -> Bool" $
      slotTest (Proxy @(Poly 1 Bool)) `shouldBe` "Bool"
    it "Poly 2 Char -> Char" $
      slotTest (Proxy @(Poly 2 Char)) `shouldBe` "Char"
    it "Poly 3 Double -> Double" $
      slotTest (Proxy @(Poly 3 Double)) `shouldBe` "Double"
    it "Poly 0 [Int] -> [Int]" $
      slotTest (Proxy @(Poly 0 [Int])) `shouldBe` "[Int]"
    it "Poly 0 (Maybe Int) -> Maybe Int" $
      slotTest (Proxy @(Poly 0 (Maybe Int))) `shouldBe` "Maybe Int"

  describe "slotUser (single Fixed slot)" $ do
    it "Fixed Int -> Int" $
      slotUser (Proxy @(Fixed Int)) `shouldBe` "Int"
    it "Fixed Bool -> Bool" $
      slotUser (Proxy @(Fixed Bool)) `shouldBe` "Bool"
    it "Fixed Char -> Char" $
      slotUser (Proxy @(Fixed Char)) `shouldBe` "Char"
    it "Fixed [Int] -> [Int]" $
      slotUser (Proxy @(Fixed [Int])) `shouldBe` "[Int]"
    it "Fixed (Maybe Bool) -> Maybe Bool" $
      slotUser (Proxy @(Fixed (Maybe Bool))) `shouldBe` "Maybe Bool"
    it "Fixed String -> [Char]" $
      slotUser (Proxy @(Fixed String)) `shouldBe` "[Char]"

  describe "slotTest (single Fixed slot)" $ do
    it "Fixed Int -> Int" $
      slotTest (Proxy @(Fixed Int)) `shouldBe` "Int"
    it "Fixed Bool -> Bool" $
      slotTest (Proxy @(Fixed Bool)) `shouldBe` "Bool"
    it "Fixed Char -> Char" $
      slotTest (Proxy @(Fixed Char)) `shouldBe` "Char"
    it "Fixed [Int] -> [Int]" $
      slotTest (Proxy @(Fixed [Int])) `shouldBe` "[Int]"
    it "Fixed (Maybe Bool) -> Maybe Bool" $
      slotTest (Proxy @(Fixed (Maybe Bool))) `shouldBe` "Maybe Bool"
    it "Fixed String -> [Char] (Typeable renders String as [Char])" $
      slotTest (Proxy @(Fixed String)) `shouldBe` "[Char]"

  ---------------------------------------------------------------------------
  -- B. reifyUser / reifyTest for slot lists
  ---------------------------------------------------------------------------
  describe "reifyUser (slot lists)" $ do
    it "empty list" $
      reifyUser (Proxy @'[]) `shouldBe` ([] :: [String])
    it "single Fixed" $
      reifyUser (Proxy @'[Fixed Int]) `shouldBe` ["Int"]
    it "single Poly" $
      reifyUser (Proxy @'[Poly 0 Int]) `shouldBe` ["a"]
    it "two same Poly" $
      reifyUser (Proxy @'[Poly 0 Int, Poly 0 Int]) `shouldBe` ["a", "a"]
    it "two different Poly" $
      reifyUser (Proxy @'[Poly 0 Int, Poly 1 Bool]) `shouldBe` ["a", "b"]
    it "two Fixed" $
      reifyUser (Proxy @'[Fixed Int, Fixed Bool]) `shouldBe` ["Int", "Bool"]
    it "Poly then Fixed" $
      reifyUser (Proxy @'[Poly 0 Int, Fixed Bool]) `shouldBe` ["a", "Bool"]
    it "Fixed then Poly" $
      reifyUser (Proxy @'[Fixed Int, Poly 0 Bool]) `shouldBe` ["Int", "a"]
    it "Example (4-slot mixed)" $
      reifyUser (Proxy @Example) `shouldBe` ["a", "a", "Char", "b"]
    it "three identical Poly" $
      reifyUser (Proxy @'[Poly 0 Int, Poly 0 Int, Poly 0 Int])
        `shouldBe` ["a", "a", "a"]
    it "4-slot alternating" $
      reifyUser (Proxy @'[Poly 0 Int, Fixed Char, Poly 1 Bool, Fixed Double])
        `shouldBe` ["a", "Char", "b", "Double"]
    it "5-slot with repeated Poly" $
      reifyUser (Proxy @'[Poly 0 Int, Poly 1 Bool, Poly 2 Char, Fixed Double, Poly 0 Int])
        `shouldBe` ["a", "b", "c", "Double", "a"]

  describe "reifyTest (slot lists)" $ do
    it "empty list" $
      reifyTest (Proxy @'[]) `shouldBe` ([] :: [String])
    it "single Fixed" $
      reifyTest (Proxy @'[Fixed Int]) `shouldBe` ["Int"]
    it "single Poly" $
      reifyTest (Proxy @'[Poly 0 Int]) `shouldBe` ["Int"]
    it "two same Poly" $
      reifyTest (Proxy @'[Poly 0 Int, Poly 0 Int]) `shouldBe` ["Int", "Int"]
    it "two different Poly" $
      reifyTest (Proxy @'[Poly 0 Int, Poly 1 Bool]) `shouldBe` ["Int", "Bool"]
    it "two Fixed" $
      reifyTest (Proxy @'[Fixed Int, Fixed Bool]) `shouldBe` ["Int", "Bool"]
    it "Poly then Fixed" $
      reifyTest (Proxy @'[Poly 0 Int, Fixed Bool]) `shouldBe` ["Int", "Bool"]
    it "Fixed then Poly" $
      reifyTest (Proxy @'[Fixed Int, Poly 0 Bool]) `shouldBe` ["Int", "Bool"]
    it "Example (4-slot mixed)" $
      reifyTest (Proxy @Example) `shouldBe` ["Int", "Int", "Char", "Bool"]
    it "three identical Poly" $
      reifyTest (Proxy @'[Poly 0 Int, Poly 0 Int, Poly 0 Int])
        `shouldBe` ["Int", "Int", "Int"]
    it "4-slot alternating" $
      reifyTest (Proxy @'[Poly 0 Int, Fixed Char, Poly 1 Bool, Fixed Double])
        `shouldBe` ["Int", "Char", "Bool", "Double"]
    it "5-slot with repeated Poly" $
      reifyTest (Proxy @'[Poly 0 Int, Poly 1 Bool, Poly 2 Char, Fixed Double, Poly 0 Int])
        `shouldBe` ["Int", "Bool", "Char", "Double", "Int"]

  ---------------------------------------------------------------------------
  -- C. showUserSig / showTestSig (arrow-joined)
  ---------------------------------------------------------------------------
  describe "showUserSig" $ do
    it "empty list" $
      showUserSig (Proxy @'[]) `shouldBe` ""
    it "single Fixed (no arrows)" $
      showUserSig (Proxy @'[Fixed Int]) `shouldBe` "Int"
    it "single Poly (no arrows)" $
      showUserSig (Proxy @'[Poly 0 Int]) `shouldBe` "a"
    it "two Poly" $
      showUserSig (Proxy @'[Poly 0 Int, Poly 1 Bool]) `shouldBe` "a -> b"
    it "Example" $
      showUserSig (Proxy @Example) `shouldBe` "a -> a -> Char -> b"
    it "all Fixed" $
      showUserSig (Proxy @'[Fixed Int, Fixed Bool, Fixed Char])
        `shouldBe` "Int -> Bool -> Char"
    it "Poly-Fixed-Poly" $
      showUserSig (Proxy @'[Poly 0 Int, Fixed Char, Poly 1 Bool])
        `shouldBe` "a -> Char -> b"

  describe "showTestSig" $ do
    it "empty list" $
      showTestSig (Proxy @'[]) `shouldBe` ""
    it "single Fixed" $
      showTestSig (Proxy @'[Fixed Int]) `shouldBe` "Int"
    it "single Poly" $
      showTestSig (Proxy @'[Poly 0 Int]) `shouldBe` "Int"
    it "two Poly" $
      showTestSig (Proxy @'[Poly 0 Int, Poly 1 Bool]) `shouldBe` "Int -> Bool"
    it "Example" $
      showTestSig (Proxy @Example) `shouldBe` "Int -> Int -> Char -> Bool"
    it "all Fixed" $
      showTestSig (Proxy @'[Fixed Int, Fixed Bool, Fixed Char])
        `shouldBe` "Int -> Bool -> Char"
    it "Poly-Fixed-Poly" $
      showTestSig (Proxy @'[Poly 0 Int, Fixed Char, Poly 1 Bool])
        `shouldBe` "Int -> Char -> Bool"

  ---------------------------------------------------------------------------
  -- D. ToConcrete type family
  ---------------------------------------------------------------------------
  describe "ToConcrete type family" $ do
    it "single Fixed -> bare type" $
      typeRep (Proxy :: Proxy (ToConcrete '[Fixed Int]))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "single Poly -> bare concrete type" $
      typeRep (Proxy :: Proxy (ToConcrete '[Poly 0 Int]))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "two slots -> function type" $
      typeRep (Proxy :: Proxy (ToConcrete '[Fixed Int, Fixed Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Bool))

    it "Poly-Poly-Fixed -> function" $
      typeRep (Proxy :: Proxy (ToConcrete '[Poly 0 Int, Poly 0 Int, Fixed Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Int -> Bool))

    it "Example type -> Int -> Int -> Char -> Bool" $
      typeRep (Proxy :: Proxy (ToConcrete Example))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Int -> Char -> Bool))

    it "four slots mixed" $
      typeRep (Proxy :: Proxy (ToConcrete '[Fixed Int, Poly 0 Bool, Fixed Char, Poly 1 Double]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Bool -> Char -> Double))
