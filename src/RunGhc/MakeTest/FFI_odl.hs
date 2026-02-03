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
import RunGhc.MakeTest.HKTs

--------------------------------------------------------------------------------
-- Extract concrete type from a Slot
--------------------------------------------------------------------------------

type family SlotType (s :: Slot) :: Type where
  SlotType ('MkSlot e t) = t

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
  -- extend as needed

--------------------------------------------------------------------------------
-- Wrapper for a signature's values
--------------------------------------------------------------------------------

newtype SigVal (xs :: [Slot]) = SigVal { unSigVal :: ToTuple xs }

instance (FromJSON (ToTuple xs)) => FromJSON (SigVal xs) where
  parseJSON v = SigVal <$> parseJSON v

instance (ToJSON (ToTuple xs)) => ToJSON (SigVal xs) where
  toJSON (SigVal x) = toJSON x
