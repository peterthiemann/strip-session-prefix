{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BSession.Nat where

import Data.Hashable
import Data.Kind
import Numeric.Natural (Natural)
import Prettyprinter

type Nat :: Type
data Nat = Z | S Nat
  deriving stock (Eq, Ord)

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

deriving stock instance Eq (Fin n)

deriving stock instance Ord (Fin n)

instance Hashable (Fin n) where
  hashWithSalt s FZ = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (FS n) = s `hashWithSalt` (1 :: Int) `hashWithSalt` n

finToNat :: Fin n -> Nat
finToNat FZ = Z
finToNat (FS n) = S (finToNat n)

class ToNum a where
  toNum :: a -> Natural

instance ToNum Nat where
  toNum = \case
    Z -> 0
    S n -> 1 + toNum n

instance ToNum (Fin n) where
  toNum = \case
    FZ -> 0
    FS n -> 1 + toNum n

instance Show Nat where
  show = show . toNum

instance Pretty Nat where
  pretty = pretty . toNum

instance Show (Fin n) where
  show = show . toNum

instance Pretty (Fin n) where
  pretty = pretty . toNum
