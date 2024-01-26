{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BSession.Nat where

import Data.Kind
import Numeric.Natural
import Prettyprinter

type Nat :: Type
data Nat where
  Z :: Nat
  S :: Nat -> Nat

deriving stock instance Eq Nat

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

deriving stock instance Eq (Fin n)

class ToNat a where
  toNat :: a -> Natural

instance ToNat Nat where
  toNat = \case
    Z -> 0
    S n -> 1 + toNat n

instance ToNat (Fin n) where
  toNat = \case
    FZ -> 0
    FS n -> 1 + toNat n

instance Pretty Nat where
  pretty = pretty . toNat

instance Pretty (Fin n) where
  pretty = pretty . toNat
