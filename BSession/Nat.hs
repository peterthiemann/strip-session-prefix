{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BSession.Nat where

import Data.Kind
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

class ToNum a where
  toNum :: (Num b) => a -> b

instance ToNum Nat where
  toNum = \case
    Z -> 0
    S n -> 1 + toNum n

instance ToNum (Fin n) where
  toNum = \case
    FZ -> 0
    FS n -> 1 + toNum n

instance Pretty Nat where
  pretty = pretty @Int . toNum

instance Pretty (Fin n) where
  pretty = pretty @Int . toNum
