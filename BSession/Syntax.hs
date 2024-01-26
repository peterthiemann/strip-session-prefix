{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BSession.Syntax where

import BSession.Nat
import Data.IntSet qualified as IS
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Prettyprinter

-- |  A `VarLabel` is the user facing name of a variable.
--
--  All `VarLabel`s compare equal! Variables should be compared by their index.
newtype VarLabel = VarLabel T.Text
  deriving newtype (Pretty)

instance Eq VarLabel where
  _ == _ = True

data Var n = Var !VarLabel !(Fin n)

instance Eq (Var n) where
  Var _ m == Var _ n = m == n

instance Pretty (Var n) where
  pretty (Var lbl n) = pretty lbl <> "@" <> pretty n

data Dir = In | Out
  deriving stock (Eq)

newtype Ty = Ty T.Text
  deriving newtype (Eq, Pretty)

type Session0 = Session Z

type Session :: Nat -> Type
data Session n where
  SEnd :: Session n
  SRet :: Session n
  SVar :: !(Var n) -> Session n
  SCom :: !Dir -> !Ty -> Session n -> Session n
  SAlt :: !Dir -> NE.NonEmpty (Session n) -> Session n
  SMu :: !VarLabel -> Session (S n) -> Session n

deriving stock instance Eq (Session n)

instance Pretty (Session n) where
  pretty = \case
    SEnd -> "end"
    SRet -> "ret"
    SVar v -> pretty v
    SCom x t s -> (case x of In -> "?"; Out -> "!") <> pretty t <+> dot <+> pretty s
    SAlt x ss -> (case x of In -> "&"; Out -> "+") <> encloseSep "{ " " }" " ; " (pretty <$> NE.toList ss)
    SMu v s -> "rec " <> pretty v <> dot <+> pretty s

contractive :: Session n -> Bool
contractive = go 0
  where
    go :: Int -> Session n -> Bool
    go !preceedingBinders = \case
      SEnd -> True
      SRet -> True
      SVar (Var _ n) -> toNum n >= preceedingBinders
      SCom _ _ s -> contractive s
      SAlt _ ss -> all contractive ss
      SMu _ s -> go (preceedingBinders + 1) s
