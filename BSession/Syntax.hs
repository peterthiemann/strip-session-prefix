{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BSession.Syntax where

import BSession.Nat
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Prettyprinter

data Var n = Var
  { varLabel :: !T.Text,
    varIndex :: !(Fin n)
  }

instance Pretty (Var n) where
  pretty v = pretty (varLabel v) <> "@" <> pretty (varIndex v)

data Dir = In | Out

newtype Ty = Ty T.Text

instance Pretty Ty where
  pretty (Ty s) = pretty s

type Session :: Nat -> Type
data Session n where
  SEnd :: Session n
  SRet :: Session n
  SVar :: Var n -> Session n
  SCom :: !Dir -> Ty -> Session n -> Session n
  SAlt :: !Dir -> NE.NonEmpty (Session n) -> Session n
  SMu :: !T.Text -> Session (S n) -> Session n

instance Pretty (Session n) where
  pretty = \case
    SEnd -> "end"
    SRet -> "ret"
    SVar v -> pretty v
    SCom x t s -> (case x of In -> "?"; Out -> "!") <> pretty t <+> dot <+> pretty s
    SAlt x ss -> (case x of In -> "&"; Out -> "+") <> encloseSep "{ " " }" " ; " (pretty <$> NE.toList ss)
    SMu v s -> "rec " <> pretty v <> dot <+> pretty s
