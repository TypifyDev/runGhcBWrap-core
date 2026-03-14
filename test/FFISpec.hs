{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module FFISpec where

import Test.Hspec
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.Proxy
import Data.Typeable (typeRep)
import RunGhc.MakeTest.HKTs
import RunGhc.MakeTest.FFI

-- | Round-trip helper: encode then decode a SigVal, extracting the inner tuple.
-- Uses ScopedTypeVariables to tie the decode type to the same slot list.
roundTrip :: forall xs. (ToJSON (ToTuple xs), FromJSON (ToTuple xs))
          => SigVal xs -> Maybe (ToTuple xs)
roundTrip v = unSigVal <$> (decode (encode v) :: Maybe (SigVal xs))

spec :: Spec
spec = describe "RunGhc.MakeTest.FFI" $ do

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — single slot
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (single slot)" $ do
    it "Fix Int" $
      roundTrip (SigVal @'[Fix Int] 42) `shouldBe` Just (42 :: Int)

    it "Fix Bool" $
      roundTrip (SigVal @'[Fix Bool] True) `shouldBe` Just True

    it "Fix Double" $
      roundTrip (SigVal @'[Fix Double] 3.14) `shouldBe` Just (3.14 :: Double)

    it "Var 0 Int" $
      roundTrip (SigVal @'[Var 0 Int] 99) `shouldBe` Just (99 :: Int)

    it "Var 1 Bool" $
      roundTrip (SigVal @'[Var 1 Bool] False) `shouldBe` Just False

    it "App1 [] 0 Int (list)" $
      roundTrip (SigVal @'[App1 [] 0 Int] [1, 2, 3 :: Int]) `shouldBe` Just [1, 2, 3 :: Int]

    it "App1Fix [] Int (fixed list)" $
      roundTrip (SigVal @'[App1Fix [] Int] [10, 20 :: Int]) `shouldBe` Just [10, 20 :: Int]

    it "App1 Maybe 0 Int (Just)" $
      roundTrip (SigVal @'[App1 Maybe 0 Int] (Just (42 :: Int))) `shouldBe` Just (Just (42 :: Int))

    it "App1 Maybe 0 Int (Nothing)" $
      roundTrip (SigVal @'[App1 Maybe 0 Int] (Nothing :: Maybe Int)) `shouldBe` Just (Nothing :: Maybe Int)

    it "App1Fix Maybe Int (fixed Maybe)" $
      roundTrip (SigVal @'[App1Fix Maybe Int] (Just (7 :: Int))) `shouldBe` Just (Just (7 :: Int))

    it "App1 [] 0 Int (empty list)" $
      roundTrip (SigVal @'[App1 [] 0 Int] ([] :: [Int])) `shouldBe` Just ([] :: [Int])

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — two slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (two slots)" $ do
    it "Var 0 Int, Fix Bool" $
      roundTrip (SigVal @'[Var 0 Int, Fix Bool] (1, True)) `shouldBe` Just (1 :: Int, True)

    it "Fix Int, Fix Bool" $
      roundTrip (SigVal @'[Fix Int, Fix Bool] (42, False)) `shouldBe` Just (42 :: Int, False)

    it "Var 0 Int, Var 1 Bool" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool] (10, True)) `shouldBe` Just (10 :: Int, True)

    it "App1 [] 0 Int, Fix Bool" $
      roundTrip (SigVal @'[App1 [] 0 Int, Fix Bool] ([1,2], True)) `shouldBe` Just ([1,2 :: Int], True)

    it "Fix Int, App1 [] 0 Bool" $
      roundTrip (SigVal @'[Fix Int, App1 [] 0 Bool] (5, [True, False])) `shouldBe` Just (5 :: Int, [True, False])

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — three slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (three slots)" $ do
    it "Var 0 Int, Var 1 Bool, Fix Char" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool, Fix Char] (1, True, 'x')) `shouldBe` Just (1 :: Int, True, 'x')

    it "Fix Int, Fix Bool, Fix Char" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char] (1, True, 'z')) `shouldBe` Just (1 :: Int, True, 'z')

    it "Var 0 Int, App1 [] 0 Int, Fix Bool" $
      roundTrip (SigVal @'[Var 0 Int, App1 [] 0 Int, Fix Bool] (5, [1,2], True)) `shouldBe` Just (5 :: Int, [1,2 :: Int], True)

    it "App1 Maybe 0 Int, Var 0 Int, Fix Bool" $
      roundTrip (SigVal @'[App1 Maybe 0 Int, Var 0 Int, Fix Bool] (Just 3, 7, False)) `shouldBe` Just (Just (3 :: Int), 7 :: Int, False)

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — four slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (four slots)" $ do
    it "Var 0 Int, Var 1 Bool, Fix Char, Fix Double" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool, Fix Char, Fix Double] (1, True, 'a', 2.5)) `shouldBe` Just (1 :: Int, True, 'a', 2.5 :: Double)

    it "Fix Int, Fix Bool, Fix Char, Fix Double" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char, Fix Double] (10, False, 'b', 1.0)) `shouldBe` Just (10 :: Int, False, 'b', 1.0 :: Double)

  ---------------------------------------------------------------------------
  -- SigVal with example types from HKTs
  ---------------------------------------------------------------------------
  describe "SigVal with example Sig types" $ do
    it "WithConstraint slots: [App1 [] 0 Int, Fix Bool]" $
      roundTrip (SigVal @'[App1 [] 0 Int, Fix Bool] ([1,2,3], True)) `shouldBe` Just ([1,2,3 :: Int], True)

    it "Simple slots: [Var 0 Int, Var 0 Int, Var 1 Bool]" $
      roundTrip (SigVal @'[Var 0 Int, Var 0 Int, Var 1 Bool] (1, 2, True)) `shouldBe` Just (1 :: Int, 2 :: Int, True)

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — five slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (five slots)" $ do
    it "all Fix" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int]
        (1, True, 'a', 2.5, 99))
        `shouldBe` Just (1 :: Int, True, 'a', 2.5 :: Double, 99 :: Int)

    it "mixed Var/Fix/App1" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool, Fix Char, Var 2 Double, Fix Int]
        (10, False, 'z', 1.1, 42))
        `shouldBe` Just (10 :: Int, False, 'z', 1.1 :: Double, 42 :: Int)

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — six slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (six slots)" $ do
    it "all Fix" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool]
        (1, True, 'a', 2.5, 99, False))
        `shouldBe` Just (1 :: Int, True, 'a', 2.5 :: Double, 99 :: Int, False)

    it "mixed slot types" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool, Var 2 Char, Fix Double, App1 [] 0 Int, Fix Bool]
        (10, True, 'c', 3.14, [1,2,3], False))
        `shouldBe` Just (10 :: Int, True, 'c', 3.14 :: Double, [1,2,3 :: Int], False)

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — seven slots
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (seven slots)" $ do
    it "all Fix" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool, Fix Char]
        (1, True, 'a', 2.5, 99, False, 'z'))
        `shouldBe` Just (1 :: Int, True, 'a', 2.5 :: Double, 99 :: Int, False, 'z')

    it "mixed slot types" $
      roundTrip (SigVal @'[Var 0 Int, Fix Bool, Var 1 Char, App1 Maybe 0 Int, Fix Double, Var 2 Bool, Fix Char]
        (5, True, 'q', Just 7, 1.5, False, 'w'))
        `shouldBe` Just (5 :: Int, True, 'q', Just (7 :: Int), 1.5 :: Double, False, 'w')

  ---------------------------------------------------------------------------
  -- SigVal JSON round-trips — eight slots (maximum ToTuple arity)
  ---------------------------------------------------------------------------
  describe "SigVal JSON round-trip (eight slots)" $ do
    it "all Fix" $
      roundTrip (SigVal @'[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool, Fix Char, Fix Double]
        (1, True, 'a', 2.5, 99, False, 'z', 3.14))
        `shouldBe` Just (1 :: Int, True, 'a', 2.5 :: Double, 99 :: Int, False, 'z', 3.14 :: Double)

    it "mixed Var/Fix/App1" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool, Fix Char, Var 2 Double, App1 [] 0 Int, Fix Bool, Var 0 Int, Fix Char]
        (10, True, 'x', 2.0, [1,2], False, 20, 'y'))
        `shouldBe` Just (10 :: Int, True, 'x', 2.0 :: Double, [1,2 :: Int], False, 20 :: Int, 'y')

  ---------------------------------------------------------------------------
  -- SigVal edge cases
  ---------------------------------------------------------------------------
  describe "SigVal edge cases" $ do
    it "empty slot list (unit)" $
      roundTrip (SigVal @'[] ()) `shouldBe` Just ()

    it "nested Fixed: Maybe [Int]" $
      roundTrip (SigVal @'[Fix (Maybe [Int])] (Just [1,2,3]))
        `shouldBe` Just (Just [1,2,3 :: Int])

    it "nested Fixed: Either Int Bool" $
      roundTrip (SigVal @'[Fix (Either Int Bool)] (Right True))
        `shouldBe` Just (Right True :: Either Int Bool)

    it "nested Fixed: (Int, Bool) tuple" $
      roundTrip (SigVal @'[Fix (Int, Bool)] (42, True))
        `shouldBe` Just (42 :: Int, True)

  ---------------------------------------------------------------------------
  -- InputTuple type family resolution
  ---------------------------------------------------------------------------
  describe "InputTuple type family" $ do
    it "2-slot sig: input is singleton" $ do
      let input = 42 :: InputTuple ('MkSig '[] '[Fix Int, Fix Bool])
      input `shouldBe` (42 :: Int)

    it "3-slot sig: input is 2-tuple" $ do
      let input = (1, True) :: InputTuple ('MkSig '[] '[Fix Int, Fix Bool, Fix Char])
      input `shouldBe` (1 :: Int, True)

    it "4-slot sig: input is 3-tuple" $ do
      let input = (1, True, 'x') :: InputTuple ('MkSig '[] '[Fix Int, Fix Bool, Fix Char, Fix Double])
      input `shouldBe` (1 :: Int, True, 'x')

    it "with Var slots" $ do
      let input = (1, 2) :: InputTuple ('MkSig '[] '[Var 0 Int, Var 0 Int, Var 1 Bool])
      input `shouldBe` (1 :: Int, 2 :: Int)

    it "with App1 slots" $ do
      let input = [1,2,3] :: InputTuple ('MkSig '[] '[App1 [] 0 Int, Fix Bool])
      input `shouldBe` [1,2,3 :: Int]

    it "constraints don't affect tuple type" $ do
      let input = [1,2] :: InputTuple ('MkSig '[ 'Cst Eq 0] '[App1 [] 0 Int, Fix Bool])
      input `shouldBe` [1,2 :: Int]

  describe "OutputType type family" $ do
    it "2-slot sig: last slot is output" $ do
      let output = True :: OutputType ('MkSig '[] '[Fix Int, Fix Bool])
      output `shouldBe` True

    it "3-slot sig" $ do
      let output = 'x' :: OutputType ('MkSig '[] '[Fix Int, Fix Bool, Fix Char])
      output `shouldBe` 'x'

    it "output is a list type" $ do
      let output = [1,2] :: OutputType ('MkSig '[] '[Fix Bool, App1 [] 0 Int])
      output `shouldBe` [1,2 :: Int]

    it "output is Maybe" $ do
      let output = Just True :: OutputType ('MkSig '[] '[Fix Int, App1 Maybe 0 Bool])
      output `shouldBe` Just True

    it "with constraints" $ do
      let output = True :: OutputType ('MkSig '[ 'Cst Eq 0] '[App1 [] 0 Int, Fix Bool])
      output `shouldBe` True

  ---------------------------------------------------------------------------
  -- RequireValid (positive path — valid combinations compile and work)
  ---------------------------------------------------------------------------
  describe "RequireValid positive path" $ do
    -- NOTE: Negative path (conflicting Var IDs like '[Var 0 Int, Var 0 Bool])
    -- produces a TypeError at compile time. Testing that requires the
    -- 'should-not-typecheck' package which is not a current dependency.

    it "same Var ID, same type is valid" $
      roundTrip (SigVal @'[Var 0 Int, Var 0 Int] (1, 2))
        `shouldBe` Just (1 :: Int, 2 :: Int)

    it "different Var IDs, different types is valid" $
      roundTrip (SigVal @'[Var 0 Int, Var 1 Bool] (1, True))
        `shouldBe` Just (1 :: Int, True)

    it "Var with App1 using same ID and consistent type" $
      roundTrip (SigVal @'[Var 0 Int, App1 [] 0 Int] (5, [1,2]))
        `shouldBe` Just (5 :: Int, [1,2 :: Int])

    it "App2 with consistent Var bindings" $
      roundTrip (SigVal @'[App2 Either 0 1 Int Bool, Var 0 Int, Var 1 Bool]
        (Left 1, 2, True))
        `shouldBe` Just (Left (1 :: Int) :: Either Int Bool, 2 :: Int, True)

  ---------------------------------------------------------------------------
  -- UnwrapIOProxy type family
  ---------------------------------------------------------------------------
  describe "UnwrapIOProxy type family" $ do
    it "IO Int -> Int" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy (IO Int)))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "IO Bool -> Bool" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy (IO Bool)))
        `shouldBe` typeRep (Proxy :: Proxy Bool)

    it "IO [Int] -> [Int]" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy (IO [Int])))
        `shouldBe` typeRep (Proxy :: Proxy [Int])

    it "Int -> Int (non-IO passthrough)" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy Int))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "Bool -> Bool (non-IO passthrough)" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy Bool))
        `shouldBe` typeRep (Proxy :: Proxy Bool)

    it "[Int] -> [Int] (non-IO passthrough)" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy [Int]))
        `shouldBe` typeRep (Proxy :: Proxy [Int])

    it "Maybe Int -> Maybe Int (non-IO passthrough)" $
      typeRep (Proxy :: Proxy (UnwrapIOProxy (Maybe Int)))
        `shouldBe` typeRep (Proxy :: Proxy (Maybe Int))

  ---------------------------------------------------------------------------
  -- SlotType type family (direct tests)
  ---------------------------------------------------------------------------
  describe "SlotType type family" $ do
    it "Fix Int -> Int" $
      typeRep (Proxy :: Proxy (SlotType (Fix Int)))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "Var 0 Int -> Int" $
      typeRep (Proxy :: Proxy (SlotType (Var 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "Var 1 Bool -> Bool" $
      typeRep (Proxy :: Proxy (SlotType (Var 1 Bool)))
        `shouldBe` typeRep (Proxy :: Proxy Bool)

    it "App1 [] 0 Int -> [Int]" $
      typeRep (Proxy :: Proxy (SlotType (App1 [] 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy [Int])

    it "App1 Maybe 0 Int -> Maybe Int" $
      typeRep (Proxy :: Proxy (SlotType (App1 Maybe 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy (Maybe Int))

    it "App2 Either 0 1 Int Bool -> Either Int Bool" $
      typeRep (Proxy :: Proxy (SlotType (App2 Either 0 1 Int Bool)))
        `shouldBe` typeRep (Proxy :: Proxy (Either Int Bool))

    it "App1Fix [] Int -> [Int]" $
      typeRep (Proxy :: Proxy (SlotType (App1Fix [] Int)))
        `shouldBe` typeRep (Proxy :: Proxy [Int])

    it "App2VarList 0 Int -> (Int, [Int])" $
      typeRep (Proxy :: Proxy (SlotType (App2VarList 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy (Int, [Int]))

    it "App1Pair [] 0 Int -> ([Int], [Int])" $
      typeRep (Proxy :: Proxy (SlotType (App1Pair [] 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy ([Int], [Int]))

    it "App1Nested Maybe [] 0 Int -> Maybe [Int]" $
      typeRep (Proxy :: Proxy (SlotType (App1Nested Maybe [] 0 Int)))
        `shouldBe` typeRep (Proxy :: Proxy (Maybe [Int]))

    it "App1ListTupleR Int 0 Bool -> [(Int, Bool)]" $
      typeRep (Proxy :: Proxy (SlotType (App1ListTupleR Int 0 Bool)))
        `shouldBe` typeRep (Proxy :: Proxy [(Int, Bool)])

    it "App2L Either Int 0 Bool -> Either Int Bool" $
      typeRep (Proxy :: Proxy (SlotType (App2L Either Int 0 Bool)))
        `shouldBe` typeRep (Proxy :: Proxy (Either Int Bool))

    it "App2R Either 0 Int Bool -> Either Bool Int" $
      typeRep (Proxy :: Proxy (SlotType (App2R Either 0 Int Bool)))
        `shouldBe` typeRep (Proxy :: Proxy (Either Bool Int))

  ---------------------------------------------------------------------------
  -- ToTuple type family (direct tests, every arity)
  ---------------------------------------------------------------------------
  describe "ToTuple type family" $ do
    it "empty -> ()" $
      typeRep (Proxy :: Proxy (ToTuple '[]))
        `shouldBe` typeRep (Proxy :: Proxy ())

    it "1 slot -> bare type" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int]))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "2 slots -> 2-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool))

    it "3 slots -> 3-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char))

    it "4 slots -> 4-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char, Fix Double]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char, Double))

    it "5 slots -> 5-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char, Double, Int))

    it "6 slots -> 6-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char, Double, Int, Bool))

    it "7 slots -> 7-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool, Fix Char]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char, Double, Int, Bool, Char))

    it "8 slots -> 8-tuple" $
      typeRep (Proxy :: Proxy (ToTuple '[Fix Int, Fix Bool, Fix Char, Fix Double, Fix Int, Fix Bool, Fix Char, Fix Double]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char, Double, Int, Bool, Char, Double))

    it "with Var slots" $
      typeRep (Proxy :: Proxy (ToTuple '[Var 0 Int, Var 1 Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool))

    it "with App1 slots" $
      typeRep (Proxy :: Proxy (ToTuple '[App1 [] 0 Int, Fix Bool]))
        `shouldBe` typeRep (Proxy :: Proxy ([Int], Bool))

  ---------------------------------------------------------------------------
  -- InputSlots / OutputSlot (tested via ToTuple composition)
  ---------------------------------------------------------------------------
  describe "InputSlots type family (via ToTuple)" $ do
    it "2 slots: input is singleton" $
      typeRep (Proxy :: Proxy (ToTuple (InputSlots '[Fix Int, Fix Bool])))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "3 slots: input is 2-tuple" $
      typeRep (Proxy :: Proxy (ToTuple (InputSlots '[Fix Int, Fix Bool, Fix Char])))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool))

    it "4 slots: input is 3-tuple" $
      typeRep (Proxy :: Proxy (ToTuple (InputSlots '[Fix Int, Fix Bool, Fix Char, Fix Double])))
        `shouldBe` typeRep (Proxy :: Proxy (Int, Bool, Char))

    it "single slot: input is empty (unit)" $
      typeRep (Proxy :: Proxy (ToTuple (InputSlots '[Fix Int])))
        `shouldBe` typeRep (Proxy :: Proxy ())

    it "with App1 slots" $
      typeRep (Proxy :: Proxy (ToTuple (InputSlots '[App1 [] 0 Int, Fix Bool, Fix Char])))
        `shouldBe` typeRep (Proxy :: Proxy ([Int], Bool))

  describe "OutputSlot type family (via SlotType)" $ do
    it "2 slots: output is last" $
      typeRep (Proxy :: Proxy (SlotType (OutputSlot '[Fix Int, Fix Bool])))
        `shouldBe` typeRep (Proxy :: Proxy Bool)

    it "3 slots: output is last" $
      typeRep (Proxy :: Proxy (SlotType (OutputSlot '[Fix Int, Fix Bool, Fix Char])))
        `shouldBe` typeRep (Proxy :: Proxy Char)

    it "single slot: output is that slot" $
      typeRep (Proxy :: Proxy (SlotType (OutputSlot '[Fix Int])))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "output with App1" $
      typeRep (Proxy :: Proxy (SlotType (OutputSlot '[Fix Int, App1 [] 0 Bool])))
        `shouldBe` typeRep (Proxy :: Proxy [Bool])
