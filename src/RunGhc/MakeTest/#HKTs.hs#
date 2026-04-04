{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RunGhc.MakeTest.HKTs where

import Data.Kind (Type, Constraint)
import GHC.TypeLits
import Data.List (intercalate)
import Data.Char (chr, ord)
import Data.Proxy
import Data.Typeable

--------------------------------------------------------------------------------
-- Core type expression language
--------------------------------------------------------------------------------

data TyExpr
  = TVar Nat                       -- type variable: a, b, c
  | TConT Type                     -- concrete type: Int, Bool
  | TCon1 (Type -> Type)           -- 1-arg constructor: [], Maybe
  | TCon2 (Type -> Type -> Type)   -- 2-arg constructor: Either
  | TApp TyExpr TyExpr             -- application: f x

--------------------------------------------------------------------------------
-- Slot: pairs a type expression with its concrete test type
--------------------------------------------------------------------------------

data Slot = MkSlot TyExpr Type

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

data Constraint' = Cst (Type -> Constraint) Nat

data Sig = MkSig [Constraint'] [Slot]

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

-- Simple variable: a
type Var (n :: Nat) (t :: Type) = 'MkSlot ('TVar n) t

-- Fixed concrete type
type Fix (t :: Type) = 'MkSlot ('TConT t) t

-- App1: [a], Maybe a
type App1 (f :: Type -> Type) (n :: Nat) (t :: Type) = 
  'MkSlot ('TApp ('TCon1 f) ('TVar n)) (f t)

-- App2: Either a b
type App2 (f :: Type -> Type -> Type) (n :: Nat) (m :: Nat) (t1 :: Type) (t2 :: Type) = 
  'MkSlot ('TApp ('TApp ('TCon2 f) ('TVar n)) ('TVar m)) (f t1 t2)

-- App2L: Either Int a — left fixed
type App2L (f :: Type -> Type -> Type) (l :: Type) (n :: Nat) (t :: Type) = 
  'MkSlot ('TApp ('TApp ('TCon2 f) ('TConT l)) ('TVar n)) (f l t)

-- App2R: Either a Int — right fixed
type App2R (f :: Type -> Type -> Type) (n :: Nat) (r :: Type) (t :: Type) = 
  'MkSlot ('TApp ('TApp ('TCon2 f) ('TVar n)) ('TConT r)) (f t r)

-- Nested App1: [[a]], Maybe [a]
type App1Nested (f :: Type -> Type) (g :: Type -> Type) (n :: Nat) (t :: Type) = 
  'MkSlot ('TApp ('TCon1 f) ('TApp ('TCon1 g) ('TVar n))) (f (g t))

-- App1 with fixed inner: [Int] -> considered fixed but using App1 structure
type App1Fix (f :: Type -> Type) (t :: Type) =
  'MkSlot ('TApp ('TCon1 f) ('TConT t)) (f t)

-- For (a, [a]) pattern: a tuple where second element is a list of the first
type App2VarList (n :: Nat) (t :: Type) = 
  'MkSlot 
    ('TApp ('TApp ('TCon2 (,)) ('TVar n)) ('TApp ('TCon1 []) ('TVar n)))
    (t, [t])
--------------------------------------------------------------------------------
-- Type-level helpers
--------------------------------------------------------------------------------

type family EqNat (a :: Nat) (b :: Nat) :: Bool where
  EqNat a a = 'True
  EqNat a b = 'False

type family EqType (a :: Type) (b :: Type) :: Bool where
  EqType a a = 'True
  EqType a b = 'False

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If 'True  t f = t
  If 'False t f = f

type family (&&) (a :: Bool) (b :: Bool) :: Bool where
  'True && 'True = 'True
  _     && _     = 'False

type family Not (b :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

--------------------------------------------------------------------------------
-- Extract all var IDs from a TyExpr
--------------------------------------------------------------------------------

type family GetVars (e :: TyExpr) :: [Nat] where
  GetVars ('TVar n)   = '[n]
  GetVars ('TConT _)  = '[]
  GetVars ('TCon1 _)  = '[]
  GetVars ('TCon2 _)  = '[]
  GetVars ('TApp f x) = Append (GetVars f) (GetVars x)

--------------------------------------------------------------------------------
-- Reify TyExpr to user-facing string
--------------------------------------------------------------------------------

class ReifyTyExpr (e :: TyExpr) where
  reifyTyExprUser :: Proxy e -> String

instance KnownNat n => ReifyTyExpr ('TVar n) where
  reifyTyExprUser _ = [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]

instance Typeable t => ReifyTyExpr ('TConT t) where
  reifyTyExprUser _ = show (typeRep (Proxy @t))

instance Typeable f => ReifyTyExpr ('TCon1 f) where
  reifyTyExprUser _ = show (typeRep (Proxy @f))

instance Typeable f => ReifyTyExpr ('TCon2 f) where
  reifyTyExprUser _ = show (typeRep (Proxy @f))

-- Special case: list application [x] instead of ([] x)
instance ReifyTyExpr x => ReifyTyExpr ('TApp ('TCon1 []) x) where
  reifyTyExprUser _ = "[" ++ reifyTyExprUser (Proxy @x) ++ "]"

-- General application
instance {-# OVERLAPPABLE #-} (ReifyTyExpr f, ReifyTyExpr x) => ReifyTyExpr ('TApp f x) where
  reifyTyExprUser _ = 
    let fStr = reifyTyExprUser (Proxy @f)
        xStr = reifyTyExprUser (Proxy @x)
        xWrapped = if ' ' `elem` xStr then "(" ++ xStr ++ ")" else xStr
    in fStr ++ " " ++ xWrapped

--------------------------------------------------------------------------------
-- SigView: kind-level parameterization for sig rendering
--------------------------------------------------------------------------------

data SigView = Poly | Concrete

--------------------------------------------------------------------------------
-- ReifySlotAs: kind-level parameterized slot rendering
--------------------------------------------------------------------------------

class ReifySlotAs (v :: SigView) (s :: Slot) where
  slotAs :: Proxy v -> Proxy s -> String

instance ReifyTyExpr e => ReifySlotAs 'Poly ('MkSlot e t) where
  slotAs _ _ = reifyTyExprUser (Proxy @e)

instance Typeable t => ReifySlotAs 'Concrete ('MkSlot e t) where
  slotAs _ _ = show (typeRep (Proxy @t))

--------------------------------------------------------------------------------
-- ReifySlotsAs: kind-level parameterized slot list rendering
--------------------------------------------------------------------------------

class ReifySlotsAs (v :: SigView) (xs :: [Slot]) where
  reifySlotsAs :: Proxy v -> Proxy xs -> [String]

instance ReifySlotsAs v '[] where
  reifySlotsAs _ _ = []

instance (ReifySlotAs v x, ReifySlotsAs v xs) => ReifySlotsAs v (x ': xs) where
  reifySlotsAs pv _ = slotAs pv (Proxy @x) : reifySlotsAs pv (Proxy @xs)

--------------------------------------------------------------------------------
-- Reify Slot (backward compat, defined via ReifySlotAs)
--------------------------------------------------------------------------------

class ReifySlot (s :: Slot) where
  slotUser :: Proxy s -> String
  slotTest :: Proxy s -> String

instance (ReifySlotAs 'Poly s, ReifySlotAs 'Concrete s) => ReifySlot s where
  slotUser = slotAs (Proxy @'Poly)
  slotTest = slotAs (Proxy @'Concrete)

--------------------------------------------------------------------------------
-- Reify Slot list (backward compat, defined via ReifySlotsAs)
--------------------------------------------------------------------------------

class ReifySlots (xs :: [Slot]) where
  reifyUser :: Proxy xs -> [String]
  reifyTest :: Proxy xs -> [String]

instance ReifySlots '[] where
  reifyUser _ = []
  reifyTest _ = []

instance (ReifySlotAs 'Poly x, ReifySlotAs 'Concrete x, ReifySlots xs) => ReifySlots (x ': xs) where
  reifyUser _ = slotAs (Proxy @'Poly) (Proxy @x) : reifyUser (Proxy @xs)
  reifyTest _ = slotAs (Proxy @'Concrete) (Proxy @x) : reifyTest (Proxy @xs)

-- List of tuples with fixed left and variable right: [(Int, a)]
type App1ListTupleR (l :: Type) (n :: Nat) (t :: Type) =
  'MkSlot
    ('TApp ('TCon1 []) ('TApp ('TApp ('TCon2 (,)) ('TConT l)) ('TVar n)))
    [(l, t)]
--------------------------------------------------------------------------------
-- Reify Constraint
--------------------------------------------------------------------------------

class ReifyConstraint (c :: Constraint') where
  reifyConstraint :: Proxy c -> String

instance KnownNat n => ReifyConstraint ('Cst Eq n) where
  reifyConstraint _ = "Eq " ++ [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]

instance KnownNat n => ReifyConstraint ('Cst Ord n) where
  reifyConstraint _ = "Ord " ++ [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]

instance KnownNat n => ReifyConstraint ('Cst Show n) where
  reifyConstraint _ = "Show " ++ [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]

instance KnownNat n => ReifyConstraint ('Cst Num n) where
  reifyConstraint _ = "Num " ++ [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]

class ReifyConstraints (cs :: [Constraint']) where
  reifyConstraints :: Proxy cs -> [String]

instance ReifyConstraints '[] where
  reifyConstraints _ = []

instance (ReifyConstraint c, ReifyConstraints cs) => ReifyConstraints (c ': cs) where
  reifyConstraints _ = reifyConstraint (Proxy @c) : reifyConstraints (Proxy @cs)

--------------------------------------------------------------------------------
-- Reify full Sig (kind-level parameterized)
--------------------------------------------------------------------------------

class ReifySigAs (v :: SigView) (s :: Sig) where
  showSigAs :: Proxy v -> Proxy s -> String
  reifySigAs :: Proxy v -> Proxy s -> [String]

instance (ReifyConstraints cs, ReifySlotsAs 'Poly slots) => ReifySigAs 'Poly ('MkSig cs slots) where
  showSigAs _ _ =
    let constraints = reifyConstraints (Proxy @cs)
        types = reifySlotsAs (Proxy @'Poly) (Proxy @slots)
        constraintPart = case constraints of
          [] -> ""
          [c] -> c ++ " => "
          cs' -> "(" ++ intercalate ", " cs' ++ ") => "
    in constraintPart ++ intercalate " -> " types
  reifySigAs _ _ = reifySlotsAs (Proxy @'Poly) (Proxy @slots)

instance ReifySlotsAs 'Concrete slots => ReifySigAs 'Concrete ('MkSig cs slots) where
  showSigAs _ _ = intercalate " -> " (reifySlotsAs (Proxy @'Concrete) (Proxy @slots))
  reifySigAs _ _ = reifySlotsAs (Proxy @'Concrete) (Proxy @slots)

--------------------------------------------------------------------------------
-- ReifySig (backward compat, defined via ReifySigAs)
--------------------------------------------------------------------------------

class ReifySig (s :: Sig) where
  showUserSig :: Proxy s -> String
  showTestSig :: Proxy s -> String
  reifyUserSig :: Proxy s -> [String]
  reifyTestSig :: Proxy s -> [String]

instance (ReifySigAs 'Poly ('MkSig cs slots), ReifySigAs 'Concrete ('MkSig cs slots))
  => ReifySig ('MkSig cs slots) where
  showUserSig p = showSigAs (Proxy @'Poly) p
  showTestSig p = showSigAs (Proxy @'Concrete) p
  reifyUserSig p = reifySigAs (Proxy @'Poly) p
  reifyTestSig p = reifySigAs (Proxy @'Concrete) p

-- Show just the output type (last slot) for user view
showUserOutputType :: ReifySig sig => Proxy sig -> String
showUserOutputType p = case reifyUserFromSig p of
  [] -> ""
  xs -> last xs

-- Show just the input types for user view
showUserInputTypes :: ReifySig sig => Proxy sig -> String
showUserInputTypes p = case reifyUserFromSig p of
  [] -> ""
  xs -> intercalate " -> " (init xs)

-- Helper to get the list of user-facing type strings from a Sig
reifyUserFromSig :: ReifySig sig => Proxy sig -> [String]
reifyUserFromSig = reifyUserSig
--------------------------------------------------------------------------------
-- ToConcrete: convert slot list to actual function type
--------------------------------------------------------------------------------

type family ToConcrete (xs :: [Slot]) :: Type where
  ToConcrete '[] = TypeError ('Text "Empty signature")
  ToConcrete '[ 'MkSlot _ t] = t
  ToConcrete ('MkSlot _ t ': xs) = t -> ToConcrete xs

--------------------------------------------------------------------------------
-- ToConcreteFromSig: extract slots from Sig and convert
--------------------------------------------------------------------------------

type family ToConcreteFromSig (s :: Sig) :: Type where
  ToConcreteFromSig ('MkSig cs slots) = ToConcrete slots

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- Simple: a -> a -> b
-- Test:   Int -> Int -> Bool
type Simple = '[ Var 0 Int, Var 0 Int, Var 1 Bool ]

-- With App1: [a] -> Maybe a -> a
-- Test:      [Int] -> Maybe Int -> Int
type WithApp1 = '[ App1 [] 0 Int, App1 Maybe 0 Int, Var 0 Int ]

-- With App2: Either a b -> a
-- Test:      Either Int Bool -> Int
type WithEither = '[ App2 Either 0 1 Int Bool, Var 0 Int ]

-- With App2L: Either String a -> a
-- Test:       Either String Int -> Int
type WithEitherL = '[ App2L Either String 0 Int, Var 0 Int ]

-- With App2R: Either a String -> a
-- Test:       Either Int String -> Int
type WithEitherR = '[ App2R Either 0 String Int, Var 0 Int ]

-- Nested: [[a]] -> [a]
-- Test:   [[Int]] -> [Int]
type Nested = '[ App1Nested [] [] 0 Int, App1 [] 0 Int ]

-- With constraint: Eq a => [a] -> Bool
type WithConstraint = 'MkSig 
  '[ 'Cst Eq 0 ] 
  '[ App1 [] 0 Int, Fix Bool ]

-- Multiple constraints: (Eq a, Ord b) => a -> b -> Bool
type MultiConstraint = 'MkSig
  '[ 'Cst Eq 0, 'Cst Ord 1 ]
  '[ Var 0 Int, Var 1 Char, Fix Bool ]

--------------------------------------------------------------------------------
-- Usage examples (in GHCi)
--------------------------------------------------------------------------------

-- >>> reifyUser (Proxy @Simple)
-- ["a", "a", "b"]

-- >>> reifyTest (Proxy @Simple)
-- ["Int", "Int", "Bool"]

-- >>> reifyUser (Proxy @WithApp1)
-- ["[a]", "Maybe a", "a"]

-- >>> reifyTest (Proxy @WithApp1)
-- ["[Int]", "Maybe Int", "Int"]

-- >>> reifyUser (Proxy @WithEither)
-- ["Either a b", "a"]

-- >>> reifyTest (Proxy @WithEither)
-- ["Either Int Bool", "Int"]

-- >>> showUserSig (Proxy @WithConstraint)
-- "Eq a => [a] -> Bool"

-- >>> showTestSig (Proxy @WithConstraint)
-- "[Int] -> Bool"

-- >>> showUserSig (Proxy @MultiConstraint)
-- "(Eq a, Ord b) => a -> b -> Bool"

-- >>> showTestSig (Proxy @MultiConstraint)
-- "Int -> Char -> Bool"

-- >>> :kind! ToConcrete Simple
-- ToConcrete Simple :: Type
-- = Int -> Int -> Bool

-- >>> :kind! ToConcreteFromSig WithConstraint
-- ToConcreteFromSig WithConstraint :: Type
-- = [Int] -> Bool


type App1Pair (f :: Type -> Type) (n :: Nat) (t :: Type) =
  'MkSlot 
    ('TApp ('TApp ('TCon2 (,)) ('TApp ('TCon1 f) ('TVar n))) ('TApp ('TCon1 f) ('TVar n)))
    (f t, f t)

-- Extract slots from Sig
type family SigSlots (s :: Sig) :: [Slot] where
  SigSlots ('MkSig cs slots) = slots

-- Special case: tuple application (a, b) instead of ((,) a b)
instance (ReifyTyExpr a, ReifyTyExpr b) => ReifyTyExpr ('TApp ('TApp ('TCon2 (,)) a) b) where
  reifyTyExprUser _ = "(" ++ reifyTyExprUser (Proxy @a) ++ ", " ++ reifyTyExprUser (Proxy @b) ++ ")"
