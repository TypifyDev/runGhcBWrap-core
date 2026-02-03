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

module RunGhc.MakeTest.TypeSig where

import Data.Kind (Type)
import GHC.TypeLits
import Data.List (intercalate)
import Data.Char (chr, ord)
import Data.Proxy
import Data.Typeable

--------------------------------------------------------------------------------
-- Type-level Slot definition
--------------------------------------------------------------------------------

data Slot 
  = Poly Nat Type    -- polymorphic: var ID + concrete test type
  | Fixed Type       -- always concrete

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
--------------------------------------------------------------------------------
-- Validation: ensure same var ID always has same type
--------------------------------------------------------------------------------

type family ValidateSlot (id :: Nat) (t :: Type) (xs :: [Slot]) :: Bool where
  ValidateSlot id t '[] = 'True
  ValidateSlot id t (Fixed _ ': xs) = ValidateSlot id t xs
  ValidateSlot id t (Poly id' t' ': xs) =
    If (EqNat id id')
       (If (EqType t t') (ValidateSlot id t xs) 'False)
       (ValidateSlot id t xs)

type family ValidateSig (xs :: [Slot]) :: Bool where
  ValidateSig '[] = 'True
  ValidateSig (Fixed _ ': xs) = ValidateSig xs
  ValidateSig (Poly id t ': xs) = ValidateSlot id t xs && ValidateSig xs

-- type family RequireValid (xs :: [Slot]) :: [Slot] where
--   RequireValid xs = If (ValidateSig xs) xs 
--     (TypeError ('Text "Mismatched types for same type variable"))

-- import GHC.TypeLits (TypeError, ErrorMessage(..))

type family FindConflict (xs :: [Slot]) :: Maybe (Nat, Type, Type) where
  FindConflict '[] = 'Nothing
  FindConflict (Fixed _ ': xs) = FindConflict xs
  FindConflict (Poly id t ': xs) = FindConflictWith id t xs (FindConflict xs)

-- Check if this (id, t) conflicts with anything in xs
-- Also takes the "rest" result in case we don't find a conflict here
type family FindConflictWith (id :: Nat) (t :: Type) (xs :: [Slot]) (rest :: Maybe (Nat, Type, Type)) :: Maybe (Nat, Type, Type) where
  FindConflictWith id t '[] rest = rest
  FindConflictWith id t (Fixed _ ': xs) rest = FindConflictWith id t xs rest
  FindConflictWith id t (Poly id' t' ': xs) rest = 
    If (EqNat id id' && Not (EqType t t'))
       ('Just '(id, t, t'))
       (FindConflictWith id t xs rest)

--------------------------------------------------------------------------------
-- Validation with good error messages
--------------------------------------------------------------------------------

type family RequireValid (xs :: [Slot]) :: [Slot] where
  RequireValid xs = RequireValid' xs (FindConflict xs)

type family RequireValid' (xs :: [Slot]) (conflict :: Maybe (Nat, Type, Type)) :: [Slot] where
  RequireValid' xs 'Nothing = xs
  RequireValid' xs ('Just '(n, t1, t2)) = TypeError
    (     'Text "Type signature conflict for type variable '"
    ':<>: 'Text "Poly " ':<>: 'ShowType n ':<>: 'Text "'"
    ':$$: 'Text ""
    ':$$: 'Text "  Used with type: " ':<>: 'ShowType t1
    ':$$: 'Text "  Also used with: " ':<>: 'ShowType t2
    ':$$: 'Text ""
    ':$$: 'Text "Each type variable must map to exactly one concrete type."
    )

-- type family RequireValid (xs :: [Slot]) :: [Slot] where
--   RequireValid xs = If (ValidateSig xs) xs
--     (TypeError 
--       (     'Text "Invalid type signature: "
--       ':$$: 'Text "Same type variable used with different concrete types."
--       ':$$: 'Text "Each type variable (Poly n t) must use the same 't' for a given 'n'."
--       ':$$: 'Text ""
--       ':$$: 'Text "Example of valid:   [Poly 0 Int, Poly 0 Int, Poly 1 Bool]"
--       ':$$: 'Text "Example of invalid: [Poly 0 Int, Poly 0 Bool]  -- 0 maps to both Int and Bool"
--       ))


    
--------------------------------------------------------------------------------
-- Convert to concrete function signature
--------------------------------------------------------------------------------

type family ToConcrete (xs :: [Slot]) :: Type where
  ToConcrete xs = ToConcrete' (RequireValid xs)

type family ToConcrete' (xs :: [Slot]) :: Type where
  ToConcrete' '[Poly _ t]       = t
  ToConcrete' '[Fixed t]        = t
  ToConcrete' (Poly _ t ': xs)  = t -> ToConcrete' xs
  ToConcrete' (Fixed t ': xs)   = t -> ToConcrete' xs

--------------------------------------------------------------------------------
-- Reify single slot to String
--------------------------------------------------------------------------------

class ReifySlot (s :: Slot) where
  slotUser :: Proxy s -> String
  slotTest :: Proxy s -> String

instance (KnownNat n, Typeable t) => ReifySlot (Poly n t) where
  slotUser _ = [chr (ord 'a' + fromIntegral (natVal (Proxy @n)))]
  slotTest _ = show (typeRep (Proxy @t))

instance Typeable t => ReifySlot (Fixed t) where
  slotUser _ = show (typeRep (Proxy @t))
  slotTest _ = show (typeRep (Proxy @t))

--------------------------------------------------------------------------------
-- Reify whole signature to [String]
--------------------------------------------------------------------------------

class ReifySig (xs :: [Slot]) where
  reifyUser :: Proxy xs -> [String]
  reifyTest :: Proxy xs -> [String]

instance ReifySig '[] where
  reifyUser _ = []
  reifyTest _ = []

instance (ReifySlot x, ReifySig xs) => ReifySig (x ': xs) where
  reifyUser _ = slotUser (Proxy @x) : reifyUser (Proxy @xs)
  reifyTest _ = slotTest (Proxy @x) : reifyTest (Proxy @xs)

--------------------------------------------------------------------------------
-- Convenience: render as "a -> b -> c"
--------------------------------------------------------------------------------

showUserSig :: ReifySig xs => Proxy xs -> String
showUserSig p = intercalate " -> " (reifyUser p)

showTestSig :: ReifySig xs => Proxy xs -> String
showTestSig p = intercalate " -> " (reifyTest p)

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

-- a -> a -> Char -> b  (user view)
-- Int -> Int -> Char -> Bool  (test view)
type Example = '[ Poly 0 Int, Poly 0 Int, Fixed Char, Poly 1 Bool ]

-- Uncomment to see compile error:
-- type Bad = '[ Poly 0 Int, Poly 0 Bool, Poly 1 Bool ]
