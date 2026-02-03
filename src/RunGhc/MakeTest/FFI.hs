{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RunGhc.MakeTest.FFI where

import Data.Aeson
import Data.Kind (Type)
import GHC.TypeLits (Nat,TypeError, ErrorMessage(..))
import RunGhc.MakeTest.HKTs
--import RunGhc.MakeTest.TypeSig (RequireValid)
--------------------------------------------------------------------------------
-- Extract concrete type from a Slot
--------------------------------------------------------------------------------

type family SlotType (s :: Slot) :: Type where
  SlotType ('MkSlot e t) = t

--------------------------------------------------------------------------------
-- Split into inputs and output
--------------------------------------------------------------------------------

type family InputSlots (xs :: [Slot]) :: [Slot] where
  InputSlots '[] = TypeError ('Text "Empty signature")
  InputSlots '[x] = '[]
  InputSlots (x ': xs) = x ': InputSlots xs

type family OutputSlot (xs :: [Slot]) :: Slot where
  OutputSlot '[] = TypeError ('Text "Empty signature")
  OutputSlot '[x] = x
  OutputSlot (x ': xs) = OutputSlot xs

--------------------------------------------------------------------------------
-- Convert slot list to tuple of concrete types
--------------------------------------------------------------------------------

type family ToTuple (xs :: [Slot]) :: Type where
  ToTuple '[] = ()
  ToTuple '[a] = SlotType a
  ToTuple '[a, b] = (SlotType a, SlotType b)
  ToTuple '[a, b, c] = (SlotType a, SlotType b, SlotType c)
  ToTuple '[a, b, c, d] = (SlotType a, SlotType b, SlotType c, SlotType d)
  ToTuple '[a, b, c, d, e] = (SlotType a, SlotType b, SlotType c, SlotType d, SlotType e)
  ToTuple '[a, b, c, d, e, f] = (SlotType a, SlotType b, SlotType c, SlotType d, SlotType e, SlotType f)
  ToTuple '[a, b, c, d, e, f, g] = (SlotType a, SlotType b, SlotType c, SlotType d, SlotType e, SlotType f, SlotType g)
  ToTuple '[a, b, c, d, e, f, g, h] = (SlotType a, SlotType b, SlotType c, SlotType d, SlotType e, SlotType f, SlotType g, SlotType h)

--------------------------------------------------------------------------------
-- InputTuple and OutputType for Sig
--------------------------------------------------------------------------------

type family InputTuple (s :: Sig) :: Type where
  InputTuple ('MkSig cs slots) = ToTuple (InputSlots (RequireValid slots))

type family OutputType (s :: Sig) :: Type where
  OutputType ('MkSig cs slots) = SlotType (OutputSlot (RequireValid slots))

--------------------------------------------------------------------------------
-- Wrapper for a signature's values
--------------------------------------------------------------------------------

newtype SigVal (xs :: [Slot]) = SigVal { unSigVal :: ToTuple xs }

instance (FromJSON (ToTuple xs)) => FromJSON (SigVal xs) where
  parseJSON v = SigVal <$> parseJSON v

instance (ToJSON (ToTuple xs)) => ToJSON (SigVal xs) where
  toJSON (SigVal x) = toJSON x 




--------------------------------------------------------------------------------
-- Extract var bindings from a TyExpr (returns list of (Nat, Type) pairs)
-- The Type is the concrete test type from the Slot, not from the TyExpr
--------------------------------------------------------------------------------

type family ExtractVars (e :: TyExpr) (t :: Type) :: [(Nat, Type)] where
  ExtractVars ('TVar n) t = '[ '(n, t) ]
  ExtractVars ('TConT _) _ = '[]
  ExtractVars ('TCon1 _) _ = '[]
  ExtractVars ('TCon2 _) _ = '[]
  ExtractVars ('TApp f x) t = Append (ExtractVarsFromApp f t) (ExtractVarsFromApp x t)

-- For TApp, we can't easily know what concrete type the inner part has
-- So we just extract the var IDs and mark them with the outer type
-- This is a simplification - for full correctness you'd need to track types through application
type family ExtractVarsFromApp (e :: TyExpr) (t :: Type) :: [(Nat, Type)] where
  ExtractVarsFromApp ('TVar n) t = '[ '(n, t) ]
  ExtractVarsFromApp ('TConT _) _ = '[]
  ExtractVarsFromApp ('TCon1 _) _ = '[]
  ExtractVarsFromApp ('TCon2 _) _ = '[]
  ExtractVarsFromApp ('TApp f x) t = Append (ExtractVarsFromApp f t) (ExtractVarsFromApp x t)

--------------------------------------------------------------------------------
-- Collect all var bindings from a slot list
--------------------------------------------------------------------------------

type family CollectVarBindings (xs :: [Slot]) :: [(Nat, Type)] where
  CollectVarBindings '[] = '[]
  CollectVarBindings ('MkSlot e t ': xs) = Append (ExtractVarsSimple e t) (CollectVarBindings xs)

-- Simplified: only check top-level TVar, not nested in TApp
-- This catches the common case: Var 0 Int, Var 0 Bool would conflict
type family ExtractVarsSimple (e :: TyExpr) (t :: Type) :: [(Nat, Type)] where
  ExtractVarsSimple ('TVar n) t = '[ '(n, t) ]
  ExtractVarsSimple ('TConT _) _ = '[]
  ExtractVarsSimple ('TCon1 _) _ = '[]
  ExtractVarsSimple ('TCon2 _) _ = '[]
  ExtractVarsSimple ('TApp ('TCon1 f) ('TVar n)) t = '[ '(n, UnwrapApp1 f t) ]
  ExtractVarsSimple ('TApp ('TApp ('TCon2 f) ('TVar n)) ('TVar m)) t = 
    '[ '(n, UnwrapApp2Fst f t), '(m, UnwrapApp2Snd f t) ]
  ExtractVarsSimple ('TApp _ _) _ = '[]  -- fallback for complex cases

-- Unwrap concrete type from App1: if t = [Int], unwrap to Int
type family UnwrapApp1 (f :: Type -> Type) (t :: Type) :: Type where
  UnwrapApp1 f (f a) = a
  UnwrapApp1 f t = t  -- fallback

-- Unwrap first type arg from App2: if t = Either Int Bool, get Int
type family UnwrapApp2Fst (f :: Type -> Type -> Type) (t :: Type) :: Type where
  UnwrapApp2Fst f (f a b) = a
  UnwrapApp2Fst f t = t  -- fallback

-- Unwrap second type arg from App2: if t = Either Int Bool, get Bool
type family UnwrapApp2Snd (f :: Type -> Type -> Type) (t :: Type) :: Type where
  UnwrapApp2Snd f (f a b) = b
  UnwrapApp2Snd f t = t  -- fallback

--------------------------------------------------------------------------------
-- Find conflict in var bindings
--------------------------------------------------------------------------------

type family FindConflictInBindings (xs :: [(Nat, Type)]) :: Maybe (Nat, Type, Type) where
  FindConflictInBindings '[] = 'Nothing
  FindConflictInBindings ('(n, t) ': xs) = FindConflictFor n t xs (FindConflictInBindings xs)

type family FindConflictFor (n :: Nat) (t :: Type) (xs :: [(Nat, Type)]) (rest :: Maybe (Nat, Type, Type)) :: Maybe (Nat, Type, Type) where
  FindConflictFor n t '[] rest = rest
  FindConflictFor n t ('(n', t') ': xs) rest =
    If (EqNat n n' && Not (EqType t t'))
       ('Just '(n, t, t'))
       (FindConflictFor n t xs rest)

--------------------------------------------------------------------------------
-- RequireValid
--------------------------------------------------------------------------------

type family FindConflict (xs :: [Slot]) :: Maybe (Nat, Type, Type) where
  FindConflict xs = FindConflictInBindings (CollectVarBindings xs)

type family RequireValid (xs :: [Slot]) :: [Slot] where
  RequireValid xs = RequireValid' xs (FindConflict xs)

type family RequireValid' (xs :: [Slot]) (conflict :: Maybe (Nat, Type, Type)) :: [Slot] where
  RequireValid' xs 'Nothing = xs
  RequireValid' xs ('Just '(n, t1, t2)) = TypeError
    (     'Text "Type signature conflict for type variable 'Var "
    ':<>: 'ShowType n ':<>: 'Text "'"
    ':$$: 'Text ""
    ':$$: 'Text "  Used with type: " ':<>: 'ShowType t1
    ':$$: 'Text "  Also used with: " ':<>: 'ShowType t2
    ':$$: 'Text ""
    ':$$: 'Text "Each type variable must map to exactly one concrete type."
    )

type family UnwrapIOProxy (a :: Type) :: Type where
  UnwrapIOProxy (m a) = a      -- IO a -> a, Maybe a -> a, [] a -> a
  UnwrapIOProxy a     = a      -- Int -> Int, Bool -> Bool
