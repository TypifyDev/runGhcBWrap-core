{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module HKTsSpec where

import Test.Hspec
import Data.Proxy
import Data.Typeable (typeRep)
import RunGhc.MakeTest.HKTs

spec :: Spec
spec = describe "RunGhc.MakeTest.HKTs" $ do

  ---------------------------------------------------------------------------
  -- I. ReifyTyExpr instances
  ---------------------------------------------------------------------------

  -- Instance 1: TVar n
  describe "ReifyTyExpr TVar" $ do
    it "TVar 0 -> a" $
      reifyTyExprUser (Proxy @('TVar 0)) `shouldBe` "a"
    it "TVar 1 -> b" $
      reifyTyExprUser (Proxy @('TVar 1)) `shouldBe` "b"
    it "TVar 2 -> c" $
      reifyTyExprUser (Proxy @('TVar 2)) `shouldBe` "c"
    it "TVar 3 -> d" $
      reifyTyExprUser (Proxy @('TVar 3)) `shouldBe` "d"
    it "TVar 25 -> z" $
      reifyTyExprUser (Proxy @('TVar 25)) `shouldBe` "z"

  -- Instance 2: TConT t
  describe "ReifyTyExpr TConT" $ do
    it "TConT Int -> Int" $
      reifyTyExprUser (Proxy @('TConT Int)) `shouldBe` "Int"
    it "TConT Bool -> Bool" $
      reifyTyExprUser (Proxy @('TConT Bool)) `shouldBe` "Bool"
    it "TConT Char -> Char" $
      reifyTyExprUser (Proxy @('TConT Char)) `shouldBe` "Char"
    it "TConT Double -> Double" $
      reifyTyExprUser (Proxy @('TConT Double)) `shouldBe` "Double"

  -- Instance 3: TCon1 f (standalone, no application)
  describe "ReifyTyExpr TCon1" $ do
    it "TCon1 [] -> []" $
      reifyTyExprUser (Proxy @('TCon1 [])) `shouldBe` "[]"
    it "TCon1 Maybe -> Maybe" $
      reifyTyExprUser (Proxy @('TCon1 Maybe)) `shouldBe` "Maybe"

  -- Instance 4: TCon2 f (standalone, no application)
  describe "ReifyTyExpr TCon2" $ do
    it "TCon2 Either -> Either" $
      reifyTyExprUser (Proxy @('TCon2 Either)) `shouldBe` "Either"
    it "TCon2 (,) -> (,)" $
      reifyTyExprUser (Proxy @('TCon2 (,))) `shouldBe` "(,)"

  -- Instance 5: TApp (TCon1 []) x — SPECIAL CASE: list bracket notation
  describe "ReifyTyExpr TApp list special case" $ do
    it "[a] — list of variable" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 []) ('TVar 0))) `shouldBe` "[a]"
    it "[Int] — list of concrete" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 []) ('TConT Int))) `shouldBe` "[Int]"
    it "[[a]] — nested list of variable" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 []) ('TApp ('TCon1 []) ('TVar 0))))
        `shouldBe` "[[a]]"
    it "[[Int]] — nested list of concrete" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 []) ('TApp ('TCon1 []) ('TConT Int))))
        `shouldBe` "[[Int]]"
    it "[[[a]]] — triple nested list" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 []) ('TApp ('TCon1 []) ('TApp ('TCon1 []) ('TVar 0)))))
        `shouldBe` "[[[a]]]"

  -- Instance 6: TApp (TApp (TCon2 (,)) a) b — SPECIAL CASE: tuple notation
  describe "ReifyTyExpr TApp tuple special case" $ do
    it "(a, b) — two variables" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TVar 0)) ('TVar 1)))
        `shouldBe` "(a, b)"
    it "(Int, a) — fixed left, variable right" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TConT Int)) ('TVar 0)))
        `shouldBe` "(Int, a)"
    it "(a, Bool) — variable left, fixed right" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TVar 0)) ('TConT Bool)))
        `shouldBe` "(a, Bool)"
    it "(Int, Bool) — both fixed" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TConT Int)) ('TConT Bool)))
        `shouldBe` "(Int, Bool)"
    it "(a, [a]) — variable left, list right" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TVar 0)) ('TApp ('TCon1 []) ('TVar 0))))
        `shouldBe` "(a, [a])"
    it "([a], [a]) — list in both positions" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 (,)) ('TApp ('TCon1 []) ('TVar 0))) ('TApp ('TCon1 []) ('TVar 0))))
        `shouldBe` "([a], [a])"

  -- Instance 7: TApp f x (OVERLAPPABLE general case)
  describe "ReifyTyExpr TApp general case" $ do
    it "Maybe a" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 Maybe) ('TVar 0)))
        `shouldBe` "Maybe a"
    it "Maybe Int" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 Maybe) ('TConT Int)))
        `shouldBe` "Maybe Int"
    it "Either a b" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 Either) ('TVar 0)) ('TVar 1)))
        `shouldBe` "Either a b"
    it "Either Int a" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 Either) ('TConT Int)) ('TVar 0)))
        `shouldBe` "Either Int a"
    it "Either a Bool" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 Either) ('TVar 0)) ('TConT Bool)))
        `shouldBe` "Either a Bool"
    it "Either Int Bool" $
      reifyTyExprUser (Proxy @('TApp ('TApp ('TCon2 Either) ('TConT Int)) ('TConT Bool)))
        `shouldBe` "Either Int Bool"
    it "Maybe (Maybe a) — multi-word arg gets parens" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 Maybe) ('TApp ('TCon1 Maybe) ('TVar 0))))
        `shouldBe` "Maybe (Maybe a)"
    it "Maybe [a] — list arg has no spaces, no parens" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 Maybe) ('TApp ('TCon1 []) ('TVar 0))))
        `shouldBe` "Maybe [a]"
    it "Maybe (Either a b) — compound arg gets parens" $
      reifyTyExprUser (Proxy @('TApp ('TCon1 Maybe) ('TApp ('TApp ('TCon2 Either) ('TVar 0)) ('TVar 1))))
        `shouldBe` "Maybe (Either a b)"

  ---------------------------------------------------------------------------
  -- II. Smart Constructors — slotUser / slotTest
  ---------------------------------------------------------------------------

  -- Var n t
  describe "Var smart constructor" $ do
    it "Var 0 Int user -> a" $
      slotUser (Proxy @(Var 0 Int)) `shouldBe` "a"
    it "Var 0 Int test -> Int" $
      slotTest (Proxy @(Var 0 Int)) `shouldBe` "Int"
    it "Var 1 Bool user -> b" $
      slotUser (Proxy @(Var 1 Bool)) `shouldBe` "b"
    it "Var 1 Bool test -> Bool" $
      slotTest (Proxy @(Var 1 Bool)) `shouldBe` "Bool"
    it "Var 2 Char user -> c" $
      slotUser (Proxy @(Var 2 Char)) `shouldBe` "c"
    it "Var 2 Char test -> Char" $
      slotTest (Proxy @(Var 2 Char)) `shouldBe` "Char"
    it "Var 0 [Int] user -> a" $
      slotUser (Proxy @(Var 0 [Int])) `shouldBe` "a"
    it "Var 0 [Int] test -> [Int]" $
      slotTest (Proxy @(Var 0 [Int])) `shouldBe` "[Int]"
    it "Var 0 (Maybe Int) user -> a" $
      slotUser (Proxy @(Var 0 (Maybe Int))) `shouldBe` "a"
    it "Var 0 (Maybe Int) test -> Maybe Int" $
      slotTest (Proxy @(Var 0 (Maybe Int))) `shouldBe` "Maybe Int"

  -- Fix t
  describe "Fix smart constructor" $ do
    it "Fix Int user -> Int" $
      slotUser (Proxy @(Fix Int)) `shouldBe` "Int"
    it "Fix Int test -> Int" $
      slotTest (Proxy @(Fix Int)) `shouldBe` "Int"
    it "Fix Bool user -> Bool" $
      slotUser (Proxy @(Fix Bool)) `shouldBe` "Bool"
    it "Fix Bool test -> Bool" $
      slotTest (Proxy @(Fix Bool)) `shouldBe` "Bool"
    it "Fix Char user -> Char" $
      slotUser (Proxy @(Fix Char)) `shouldBe` "Char"
    it "Fix Char test -> Char" $
      slotTest (Proxy @(Fix Char)) `shouldBe` "Char"
    it "Fix [Int] user -> [Int]" $
      slotUser (Proxy @(Fix [Int])) `shouldBe` "[Int]"
    it "Fix [Int] test -> [Int]" $
      slotTest (Proxy @(Fix [Int])) `shouldBe` "[Int]"
    it "Fix (Maybe Bool) user -> Maybe Bool" $
      slotUser (Proxy @(Fix (Maybe Bool))) `shouldBe` "Maybe Bool"
    it "Fix (Maybe Bool) test -> Maybe Bool" $
      slotTest (Proxy @(Fix (Maybe Bool))) `shouldBe` "Maybe Bool"
    it "Fix Double user -> Double" $
      slotUser (Proxy @(Fix Double)) `shouldBe` "Double"
    it "Fix Double test -> Double" $
      slotTest (Proxy @(Fix Double)) `shouldBe` "Double"

  -- App1 f n t
  describe "App1 smart constructor" $ do
    it "App1 [] 0 Int user -> [a]" $
      slotUser (Proxy @(App1 [] 0 Int)) `shouldBe` "[a]"
    it "App1 [] 0 Int test -> [Int]" $
      slotTest (Proxy @(App1 [] 0 Int)) `shouldBe` "[Int]"
    it "App1 [] 1 Bool user -> [b]" $
      slotUser (Proxy @(App1 [] 1 Bool)) `shouldBe` "[b]"
    it "App1 [] 1 Bool test -> [Bool]" $
      slotTest (Proxy @(App1 [] 1 Bool)) `shouldBe` "[Bool]"
    it "App1 Maybe 0 Int user -> Maybe a" $
      slotUser (Proxy @(App1 Maybe 0 Int)) `shouldBe` "Maybe a"
    it "App1 Maybe 0 Int test -> Maybe Int" $
      slotTest (Proxy @(App1 Maybe 0 Int)) `shouldBe` "Maybe Int"
    it "App1 Maybe 1 Bool user -> Maybe b" $
      slotUser (Proxy @(App1 Maybe 1 Bool)) `shouldBe` "Maybe b"
    it "App1 Maybe 1 Bool test -> Maybe Bool" $
      slotTest (Proxy @(App1 Maybe 1 Bool)) `shouldBe` "Maybe Bool"
    it "App1 [] 0 Char user -> [a]" $
      slotUser (Proxy @(App1 [] 0 Char)) `shouldBe` "[a]"
    it "App1 [] 0 Char test -> [Char]" $
      slotTest (Proxy @(App1 [] 0 Char)) `shouldBe` "[Char]"

  -- App2 f n m t1 t2
  describe "App2 smart constructor" $ do
    it "App2 Either 0 1 Int Bool user -> Either a b" $
      slotUser (Proxy @(App2 Either 0 1 Int Bool)) `shouldBe` "Either a b"
    it "App2 Either 0 1 Int Bool test -> Either Int Bool" $
      slotTest (Proxy @(App2 Either 0 1 Int Bool)) `shouldBe` "Either Int Bool"
    it "App2 Either 1 0 Bool Int user -> Either b a" $
      slotUser (Proxy @(App2 Either 1 0 Bool Int)) `shouldBe` "Either b a"
    it "App2 Either 1 0 Bool Int test -> Either Bool Int" $
      slotTest (Proxy @(App2 Either 1 0 Bool Int)) `shouldBe` "Either Bool Int"
    it "App2 (,) 0 1 Int Bool user -> (a, b)" $
      slotUser (Proxy @(App2 (,) 0 1 Int Bool)) `shouldBe` "(a, b)"
    it "App2 (,) 0 1 Int Bool test" $
      slotTest (Proxy @(App2 (,) 0 1 Int Bool)) `shouldBe` "(Int,Bool)"
    it "App2 Either 0 1 Char Double user -> Either a b" $
      slotUser (Proxy @(App2 Either 0 1 Char Double)) `shouldBe` "Either a b"
    it "App2 Either 0 1 Char Double test -> Either Char Double" $
      slotTest (Proxy @(App2 Either 0 1 Char Double)) `shouldBe` "Either Char Double"

  -- App2L f l n t
  describe "App2L smart constructor" $ do
    it "App2L Either String 0 Int user -> Either [Char] a" $
      slotUser (Proxy @(App2L Either String 0 Int)) `shouldBe` "Either [Char] a"
    it "App2L Either String 0 Int test -> Either [Char] Int" $
      slotTest (Proxy @(App2L Either String 0 Int)) `shouldBe` "Either [Char] Int"
    it "App2L Either Int 0 Bool user -> Either Int a" $
      slotUser (Proxy @(App2L Either Int 0 Bool)) `shouldBe` "Either Int a"
    it "App2L Either Int 0 Bool test -> Either Int Bool" $
      slotTest (Proxy @(App2L Either Int 0 Bool)) `shouldBe` "Either Int Bool"
    it "App2L (,) Int 0 Bool user -> (Int, a)" $
      slotUser (Proxy @(App2L (,) Int 0 Bool)) `shouldBe` "(Int, a)"
    it "App2L (,) Int 0 Bool test" $
      slotTest (Proxy @(App2L (,) Int 0 Bool)) `shouldBe` "(Int,Bool)"

  -- App2R f n r t
  describe "App2R smart constructor" $ do
    it "App2R Either 0 String Int user -> Either a [Char]" $
      slotUser (Proxy @(App2R Either 0 String Int)) `shouldBe` "Either a [Char]"
    it "App2R Either 0 String Int test -> Either Int [Char]" $
      slotTest (Proxy @(App2R Either 0 String Int)) `shouldBe` "Either Int [Char]"
    it "App2R Either 0 Int Bool user -> Either a Int" $
      slotUser (Proxy @(App2R Either 0 Int Bool)) `shouldBe` "Either a Int"
    it "App2R Either 0 Int Bool test -> Either Bool Int" $
      slotTest (Proxy @(App2R Either 0 Int Bool)) `shouldBe` "Either Bool Int"
    it "App2R (,) 0 Int Bool user -> (a, Int)" $
      slotUser (Proxy @(App2R (,) 0 Int Bool)) `shouldBe` "(a, Int)"
    it "App2R (,) 0 Int Bool test" $
      slotTest (Proxy @(App2R (,) 0 Int Bool)) `shouldBe` "(Bool,Int)"

  -- App1Nested f g n t
  describe "App1Nested smart constructor" $ do
    it "App1Nested [] [] 0 Int user -> [[a]]" $
      slotUser (Proxy @(App1Nested [] [] 0 Int)) `shouldBe` "[[a]]"
    it "App1Nested [] [] 0 Int test -> [[Int]]" $
      slotTest (Proxy @(App1Nested [] [] 0 Int)) `shouldBe` "[[Int]]"
    it "App1Nested Maybe [] 0 Int user -> Maybe [a]" $
      slotUser (Proxy @(App1Nested Maybe [] 0 Int)) `shouldBe` "Maybe [a]"
    it "App1Nested Maybe [] 0 Int test -> Maybe [Int]" $
      slotTest (Proxy @(App1Nested Maybe [] 0 Int)) `shouldBe` "Maybe [Int]"
    it "App1Nested [] Maybe 0 Int user -> [Maybe a]" $
      slotUser (Proxy @(App1Nested [] Maybe 0 Int)) `shouldBe` "[Maybe a]"
    it "App1Nested [] Maybe 0 Int test -> [Maybe Int]" $
      slotTest (Proxy @(App1Nested [] Maybe 0 Int)) `shouldBe` "[Maybe Int]"
    it "App1Nested Maybe Maybe 0 Int user -> Maybe (Maybe a)" $
      slotUser (Proxy @(App1Nested Maybe Maybe 0 Int)) `shouldBe` "Maybe (Maybe a)"
    it "App1Nested Maybe Maybe 0 Int test -> Maybe (Maybe Int)" $
      slotTest (Proxy @(App1Nested Maybe Maybe 0 Int)) `shouldBe` "Maybe (Maybe Int)"

  -- App1Fix f t
  describe "App1Fix smart constructor" $ do
    it "App1Fix [] Int user -> [Int]" $
      slotUser (Proxy @(App1Fix [] Int)) `shouldBe` "[Int]"
    it "App1Fix [] Int test -> [Int]" $
      slotTest (Proxy @(App1Fix [] Int)) `shouldBe` "[Int]"
    it "App1Fix Maybe Int user -> Maybe Int" $
      slotUser (Proxy @(App1Fix Maybe Int)) `shouldBe` "Maybe Int"
    it "App1Fix Maybe Int test -> Maybe Int" $
      slotTest (Proxy @(App1Fix Maybe Int)) `shouldBe` "Maybe Int"
    it "App1Fix [] Bool user -> [Bool]" $
      slotUser (Proxy @(App1Fix [] Bool)) `shouldBe` "[Bool]"
    it "App1Fix [] Bool test -> [Bool]" $
      slotTest (Proxy @(App1Fix [] Bool)) `shouldBe` "[Bool]"
    it "App1Fix Maybe Char user -> Maybe Char" $
      slotUser (Proxy @(App1Fix Maybe Char)) `shouldBe` "Maybe Char"
    it "App1Fix Maybe Char test -> Maybe Char" $
      slotTest (Proxy @(App1Fix Maybe Char)) `shouldBe` "Maybe Char"
    it "App1Fix [] [Int] user -> [[Int]]" $
      slotUser (Proxy @(App1Fix [] [Int])) `shouldBe` "[[Int]]"
    it "App1Fix [] [Int] test -> [[Int]]" $
      slotTest (Proxy @(App1Fix [] [Int])) `shouldBe` "[[Int]]"

  -- App2VarList n t
  describe "App2VarList smart constructor" $ do
    it "App2VarList 0 Int user -> (a, [a])" $
      slotUser (Proxy @(App2VarList 0 Int)) `shouldBe` "(a, [a])"
    it "App2VarList 0 Int test" $
      slotTest (Proxy @(App2VarList 0 Int)) `shouldBe` "(Int,[Int])"
    it "App2VarList 1 Bool user -> (b, [b])" $
      slotUser (Proxy @(App2VarList 1 Bool)) `shouldBe` "(b, [b])"
    it "App2VarList 1 Bool test" $
      slotTest (Proxy @(App2VarList 1 Bool)) `shouldBe` "(Bool,[Bool])"

  -- App1Pair f n t
  describe "App1Pair smart constructor" $ do
    it "App1Pair [] 0 Int user -> ([a], [a])" $
      slotUser (Proxy @(App1Pair [] 0 Int)) `shouldBe` "([a], [a])"
    it "App1Pair [] 0 Int test" $
      slotTest (Proxy @(App1Pair [] 0 Int)) `shouldBe` "([Int],[Int])"
    it "App1Pair Maybe 0 Int user -> (Maybe a, Maybe a)" $
      slotUser (Proxy @(App1Pair Maybe 0 Int)) `shouldBe` "(Maybe a, Maybe a)"
    it "App1Pair Maybe 0 Int test" $
      slotTest (Proxy @(App1Pair Maybe 0 Int)) `shouldBe` "((Maybe Int),(Maybe Int))"

  -- App1ListTupleR l n t
  describe "App1ListTupleR smart constructor" $ do
    it "App1ListTupleR Int 0 Bool user -> [(Int, a)]" $
      slotUser (Proxy @(App1ListTupleR Int 0 Bool)) `shouldBe` "[(Int, a)]"
    it "App1ListTupleR Int 0 Bool test" $
      slotTest (Proxy @(App1ListTupleR Int 0 Bool)) `shouldBe` "[(Int,Bool)]"
    it "App1ListTupleR Char 1 Int user -> [(Char, b)]" $
      slotUser (Proxy @(App1ListTupleR Char 1 Int)) `shouldBe` "[(Char, b)]"
    it "App1ListTupleR Char 1 Int test" $
      slotTest (Proxy @(App1ListTupleR Char 1 Int)) `shouldBe` "[(Char,Int)]"

  ---------------------------------------------------------------------------
  -- III. ReifySlots — multi-slot lists with mixed smart constructors
  ---------------------------------------------------------------------------
  describe "reifyUser (multi-slot mixed lists)" $ do
    it "empty" $
      reifyUser (Proxy @'[]) `shouldBe` ([] :: [String])
    it "single Var" $
      reifyUser (Proxy @'[Var 0 Int]) `shouldBe` ["a"]
    it "Var + App1 + Fix" $
      reifyUser (Proxy @'[Var 0 Int, App1 [] 0 Int, Fix Bool])
        `shouldBe` ["a", "[a]", "Bool"]
    it "App1Nested + App1" $
      reifyUser (Proxy @'[App1Nested [] [] 0 Int, App1 [] 0 Int])
        `shouldBe` ["[[a]]", "[a]"]
    it "App2 + Var" $
      reifyUser (Proxy @'[App2 Either 0 1 Int Bool, Var 0 Int])
        `shouldBe` ["Either a b", "a"]
    it "App1Fix + Fix" $
      reifyUser (Proxy @'[App1Fix [] Int, Fix Bool])
        `shouldBe` ["[Int]", "Bool"]
    it "App2VarList + Fix" $
      reifyUser (Proxy @'[App2VarList 0 Int, Fix Bool])
        `shouldBe` ["(a, [a])", "Bool"]
    it "App1Pair + Var" $
      reifyUser (Proxy @'[App1Pair [] 0 Int, Var 0 Int])
        `shouldBe` ["([a], [a])", "a"]
    it "App1ListTupleR + App1" $
      reifyUser (Proxy @'[App1ListTupleR Int 0 Bool, App1 [] 0 Bool])
        `shouldBe` ["[(Int, a)]", "[a]"]
    it "all Fix" $
      reifyUser (Proxy @'[Fix Int, Fix Bool, Fix Char])
        `shouldBe` ["Int", "Bool", "Char"]
    it "all same Var" $
      reifyUser (Proxy @'[Var 0 Int, Var 0 Int, Var 0 Int])
        `shouldBe` ["a", "a", "a"]
    it "App2L + Var" $
      reifyUser (Proxy @'[App2L Either Int 0 Bool, Var 0 Bool])
        `shouldBe` ["Either Int a", "a"]
    it "App2R + Var" $
      reifyUser (Proxy @'[App2R Either 0 Int Bool, Var 0 Bool])
        `shouldBe` ["Either a Int", "a"]
    it "App1Nested Maybe Maybe + Var" $
      reifyUser (Proxy @'[App1Nested Maybe Maybe 0 Int, Var 0 Int])
        `shouldBe` ["Maybe (Maybe a)", "a"]

  describe "reifyTest (multi-slot mixed lists)" $ do
    it "empty" $
      reifyTest (Proxy @'[]) `shouldBe` ([] :: [String])
    it "Var + App1 + Fix" $
      reifyTest (Proxy @'[Var 0 Int, App1 [] 0 Int, Fix Bool])
        `shouldBe` ["Int", "[Int]", "Bool"]
    it "App1Nested + App1" $
      reifyTest (Proxy @'[App1Nested [] [] 0 Int, App1 [] 0 Int])
        `shouldBe` ["[[Int]]", "[Int]"]
    it "App2 + Var" $
      reifyTest (Proxy @'[App2 Either 0 1 Int Bool, Var 0 Int])
        `shouldBe` ["Either Int Bool", "Int"]
    it "all Fix" $
      reifyTest (Proxy @'[Fix Int, Fix Bool, Fix Char])
        `shouldBe` ["Int", "Bool", "Char"]

  ---------------------------------------------------------------------------
  -- IV. ReifyConstraint — every instance
  ---------------------------------------------------------------------------
  describe "ReifyConstraint (individual)" $ do
    it "Eq a" $
      reifyConstraint (Proxy @('Cst Eq 0)) `shouldBe` "Eq a"
    it "Eq b" $
      reifyConstraint (Proxy @('Cst Eq 1)) `shouldBe` "Eq b"
    it "Eq c" $
      reifyConstraint (Proxy @('Cst Eq 2)) `shouldBe` "Eq c"
    it "Ord a" $
      reifyConstraint (Proxy @('Cst Ord 0)) `shouldBe` "Ord a"
    it "Ord b" $
      reifyConstraint (Proxy @('Cst Ord 1)) `shouldBe` "Ord b"
    it "Show a" $
      reifyConstraint (Proxy @('Cst Show 0)) `shouldBe` "Show a"
    it "Show b" $
      reifyConstraint (Proxy @('Cst Show 1)) `shouldBe` "Show b"
    it "Num a" $
      reifyConstraint (Proxy @('Cst Num 0)) `shouldBe` "Num a"
    it "Num b" $
      reifyConstraint (Proxy @('Cst Num 1)) `shouldBe` "Num b"
    it "Num c" $
      reifyConstraint (Proxy @('Cst Num 2)) `shouldBe` "Num c"

  ---------------------------------------------------------------------------
  -- V. ReifyConstraints — constraint lists
  ---------------------------------------------------------------------------
  describe "ReifyConstraints (lists)" $ do
    it "empty" $
      reifyConstraints (Proxy @'[]) `shouldBe` ([] :: [String])
    it "single Eq" $
      reifyConstraints (Proxy @'[ 'Cst Eq 0]) `shouldBe` ["Eq a"]
    it "Eq + Ord different vars" $
      reifyConstraints (Proxy @'[ 'Cst Eq 0, 'Cst Ord 1])
        `shouldBe` ["Eq a", "Ord b"]
    it "Eq + Ord same var" $
      reifyConstraints (Proxy @'[ 'Cst Eq 0, 'Cst Ord 0])
        `shouldBe` ["Eq a", "Ord a"]
    it "three constraints" $
      reifyConstraints (Proxy @'[ 'Cst Show 0, 'Cst Num 0, 'Cst Eq 1])
        `shouldBe` ["Show a", "Num a", "Eq b"]
    it "four constraints all different vars" $
      reifyConstraints (Proxy @'[ 'Cst Eq 0, 'Cst Ord 1, 'Cst Show 2, 'Cst Num 3])
        `shouldBe` ["Eq a", "Ord b", "Show c", "Num d"]

  ---------------------------------------------------------------------------
  -- VI. ReifySig — full Sig with constraints
  ---------------------------------------------------------------------------
  describe "showUserSig (Sig)" $ do
    it "no constraints" $
      showUserSig (Proxy @('MkSig '[] '[Var 0 Int, Fix Bool]))
        `shouldBe` "a -> Bool"
    it "single constraint" $
      showUserSig (Proxy @WithConstraint)
        `shouldBe` "Eq a => [a] -> Bool"
    it "two constraints" $
      showUserSig (Proxy @MultiConstraint)
        `shouldBe` "(Eq a, Ord b) => a -> b -> Bool"
    it "three constraints" $
      showUserSig (Proxy @('MkSig '[ 'Cst Show 0, 'Cst Num 0, 'Cst Eq 1] '[Var 0 Int, Var 1 Bool, Fix Char]))
        `shouldBe` "(Show a, Num a, Eq b) => a -> b -> Char"
    it "no constraints all Fix" $
      showUserSig (Proxy @('MkSig '[] '[Fix Int, Fix Bool]))
        `shouldBe` "Int -> Bool"
    it "single slot" $
      showUserSig (Proxy @('MkSig '[] '[Fix Int]))
        `shouldBe` "Int"
    it "empty slots" $
      showUserSig (Proxy @('MkSig '[] '[]))
        `shouldBe` ""

  describe "showTestSig (Sig)" $ do
    it "no constraints" $
      showTestSig (Proxy @('MkSig '[] '[Var 0 Int, Fix Bool]))
        `shouldBe` "Int -> Bool"
    it "single constraint (constraints stripped in test)" $
      showTestSig (Proxy @WithConstraint)
        `shouldBe` "[Int] -> Bool"
    it "two constraints" $
      showTestSig (Proxy @MultiConstraint)
        `shouldBe` "Int -> Char -> Bool"
    it "three constraints" $
      showTestSig (Proxy @('MkSig '[ 'Cst Show 0, 'Cst Num 0, 'Cst Eq 1] '[Var 0 Int, Var 1 Bool, Fix Char]))
        `shouldBe` "Int -> Bool -> Char"
    it "no constraints all Fix" $
      showTestSig (Proxy @('MkSig '[] '[Fix Int, Fix Bool]))
        `shouldBe` "Int -> Bool"
    it "single slot" $
      showTestSig (Proxy @('MkSig '[] '[Fix Int]))
        `shouldBe` "Int"
    it "empty slots" $
      showTestSig (Proxy @('MkSig '[] '[]))
        `shouldBe` ""

  describe "reifyUserSig / reifyTestSig (list versions)" $ do
    it "reifyUserSig no constraints" $
      reifyUserSig (Proxy @('MkSig '[] '[Var 0 Int, Fix Bool]))
        `shouldBe` ["a", "Bool"]
    it "reifyTestSig no constraints" $
      reifyTestSig (Proxy @('MkSig '[] '[Var 0 Int, Fix Bool]))
        `shouldBe` ["Int", "Bool"]
    it "reifyUserSig with constraints" $
      reifyUserSig (Proxy @WithConstraint)
        `shouldBe` ["[a]", "Bool"]
    it "reifyTestSig with constraints" $
      reifyTestSig (Proxy @WithConstraint)
        `shouldBe` ["[Int]", "Bool"]
    it "reifyUserSig empty" $
      reifyUserSig (Proxy @('MkSig '[] '[]))
        `shouldBe` ([] :: [String])
    it "reifyTestSig empty" $
      reifyTestSig (Proxy @('MkSig '[] '[]))
        `shouldBe` ([] :: [String])

  ---------------------------------------------------------------------------
  -- VII. showUserOutputType / showUserInputTypes
  ---------------------------------------------------------------------------
  describe "showUserOutputType" $ do
    it "WithConstraint -> Bool" $
      showUserOutputType (Proxy @WithConstraint) `shouldBe` "Bool"
    it "MultiConstraint -> Bool" $
      showUserOutputType (Proxy @MultiConstraint) `shouldBe` "Bool"
    it "3-slot: a -> [a] -> Bool" $
      showUserOutputType (Proxy @('MkSig '[] '[Var 0 Int, App1 [] 0 Int, Fix Bool]))
        `shouldBe` "Bool"
    it "single slot" $
      showUserOutputType (Proxy @('MkSig '[] '[Fix Int]))
        `shouldBe` "Int"
    it "empty -> empty string" $
      showUserOutputType (Proxy @('MkSig '[] '[]))
        `shouldBe` ""
    it "4-slot" $
      showUserOutputType (Proxy @('MkSig '[] '[Var 0 Int, Var 1 Bool, Fix Char, Fix Double]))
        `shouldBe` "Double"

  describe "showUserInputTypes" $ do
    it "WithConstraint -> [a]" $
      showUserInputTypes (Proxy @WithConstraint) `shouldBe` "[a]"
    it "MultiConstraint -> a -> b" $
      showUserInputTypes (Proxy @MultiConstraint) `shouldBe` "a -> b"
    it "3-slot: a -> [a]" $
      showUserInputTypes (Proxy @('MkSig '[] '[Var 0 Int, App1 [] 0 Int, Fix Bool]))
        `shouldBe` "a -> [a]"
    it "single slot -> empty (no inputs)" $
      showUserInputTypes (Proxy @('MkSig '[] '[Fix Int]))
        `shouldBe` ""
    it "empty -> empty string" $
      showUserInputTypes (Proxy @('MkSig '[] '[]))
        `shouldBe` ""
    it "4-slot: a -> b -> Char" $
      showUserInputTypes (Proxy @('MkSig '[] '[Var 0 Int, Var 1 Bool, Fix Char, Fix Double]))
        `shouldBe` "a -> b -> Char"

  ---------------------------------------------------------------------------
  -- VIII. Existing example types — verify GHCi doc comments
  ---------------------------------------------------------------------------
  describe "example types (verify doc comments)" $ do
    it "reifyUser Simple" $
      reifyUser (Proxy @Simple) `shouldBe` ["a", "a", "b"]
    it "reifyTest Simple" $
      reifyTest (Proxy @Simple) `shouldBe` ["Int", "Int", "Bool"]
    it "reifyUser WithApp1" $
      reifyUser (Proxy @WithApp1) `shouldBe` ["[a]", "Maybe a", "a"]
    it "reifyTest WithApp1" $
      reifyTest (Proxy @WithApp1) `shouldBe` ["[Int]", "Maybe Int", "Int"]
    it "reifyUser WithEither" $
      reifyUser (Proxy @WithEither) `shouldBe` ["Either a b", "a"]
    it "reifyTest WithEither" $
      reifyTest (Proxy @WithEither) `shouldBe` ["Either Int Bool", "Int"]
    it "reifyUser WithEitherL" $
      reifyUser (Proxy @WithEitherL) `shouldBe` ["Either [Char] a", "a"]
    it "reifyTest WithEitherL" $
      reifyTest (Proxy @WithEitherL) `shouldBe` ["Either [Char] Int", "Int"]
    it "reifyUser WithEitherR" $
      reifyUser (Proxy @WithEitherR) `shouldBe` ["Either a [Char]", "a"]
    it "reifyTest WithEitherR" $
      reifyTest (Proxy @WithEitherR) `shouldBe` ["Either Int [Char]", "Int"]
    it "reifyUser Nested" $
      reifyUser (Proxy @Nested) `shouldBe` ["[[a]]", "[a]"]
    it "reifyTest Nested" $
      reifyTest (Proxy @Nested) `shouldBe` ["[[Int]]", "[Int]"]
    it "showUserSig WithConstraint" $
      showUserSig (Proxy @WithConstraint) `shouldBe` "Eq a => [a] -> Bool"
    it "showTestSig WithConstraint" $
      showTestSig (Proxy @WithConstraint) `shouldBe` "[Int] -> Bool"
    it "showUserSig MultiConstraint" $
      showUserSig (Proxy @MultiConstraint) `shouldBe` "(Eq a, Ord b) => a -> b -> Bool"
    it "showTestSig MultiConstraint" $
      showTestSig (Proxy @MultiConstraint) `shouldBe` "Int -> Char -> Bool"

  ---------------------------------------------------------------------------
  -- VIII-b. showSigAs (kind-level parameterized)
  ---------------------------------------------------------------------------
  describe "showSigAs (kind-level parameterized)" $ do
    it "Poly matches showUserSig" $
      showSigAs (Proxy @'Poly) (Proxy @WithConstraint)
        `shouldBe` showUserSig (Proxy @WithConstraint)
    it "Concrete matches showTestSig" $
      showSigAs (Proxy @'Concrete) (Proxy @WithConstraint)
        `shouldBe` showTestSig (Proxy @WithConstraint)
    it "Poly with no constraints" $
      showSigAs (Proxy @'Poly) (Proxy @('MkSig '[] '[App1 [] 0 Int, Var 0 Int]))
        `shouldBe` "[a] -> a"
    it "Concrete with no constraints" $
      showSigAs (Proxy @'Concrete) (Proxy @('MkSig '[] '[App1 [] 0 Int, Var 0 Int]))
        `shouldBe` "[Int] -> Int"
    it "Poly with multiple constraints" $
      showSigAs (Proxy @'Poly) (Proxy @MultiConstraint)
        `shouldBe` "(Eq a, Ord b) => a -> b -> Bool"
    it "Concrete with multiple constraints" $
      showSigAs (Proxy @'Concrete) (Proxy @MultiConstraint)
        `shouldBe` "Int -> Char -> Bool"
    it "Poly all Fix" $
      showSigAs (Proxy @'Poly) (Proxy @('MkSig '[] '[Fix Int, Fix Bool]))
        `shouldBe` "Int -> Bool"
    it "Concrete all Fix" $
      showSigAs (Proxy @'Concrete) (Proxy @('MkSig '[] '[Fix Int, Fix Bool]))
        `shouldBe` "Int -> Bool"
    it "Poly single slot" $
      showSigAs (Proxy @'Poly) (Proxy @('MkSig '[] '[Fix Int]))
        `shouldBe` "Int"
    it "reifySigAs Poly" $
      reifySigAs (Proxy @'Poly) (Proxy @WithConstraint)
        `shouldBe` ["[a]", "Bool"]
    it "reifySigAs Concrete" $
      reifySigAs (Proxy @'Concrete) (Proxy @WithConstraint)
        `shouldBe` ["[Int]", "Bool"]

  ---------------------------------------------------------------------------
  -- IX. ToConcrete type family
  ---------------------------------------------------------------------------
  describe "ToConcrete type family" $ do
    it "single slot -> bare type" $
      typeRep (Proxy :: Proxy (ToConcrete '[Var 0 Int]))
        `shouldBe` typeRep (Proxy :: Proxy Int)

    it "two slots -> function type" $
      typeRep (Proxy :: Proxy (ToConcrete '[Fix Int, Fix Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Bool))

    it "three slots -> curried function" $
      typeRep (Proxy :: Proxy (ToConcrete '[Var 0 Int, Var 0 Int, Var 1 Bool]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Int -> Bool))

    it "with App1 slots" $
      typeRep (Proxy :: Proxy (ToConcrete '[App1 [] 0 Int, App1 Maybe 0 Int, Var 0 Int]))
        `shouldBe` typeRep (Proxy :: Proxy ([Int] -> Maybe Int -> Int))

    it "four slots" $
      typeRep (Proxy :: Proxy (ToConcrete '[Fix Int, Fix Bool, Fix Char, Fix Double]))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Bool -> Char -> Double))

  ---------------------------------------------------------------------------
  -- X. ToConcreteFromSig type family
  ---------------------------------------------------------------------------
  describe "ToConcreteFromSig type family" $ do
    it "simple sig" $
      typeRep (Proxy :: Proxy (ToConcreteFromSig ('MkSig '[] '[Fix Int, Fix Bool])))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Bool))

    it "WithConstraint (constraints don't affect concrete type)" $
      typeRep (Proxy :: Proxy (ToConcreteFromSig WithConstraint))
        `shouldBe` typeRep (Proxy :: Proxy ([Int] -> Bool))

    it "MultiConstraint" $
      typeRep (Proxy :: Proxy (ToConcreteFromSig MultiConstraint))
        `shouldBe` typeRep (Proxy :: Proxy (Int -> Char -> Bool))
