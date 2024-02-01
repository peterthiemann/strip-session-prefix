{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module BSession.Syntax
  ( -- * Session Types
    Session (..),
    Dir (..),
    Ty (..),

    -- ** Endings
    END (..),
    RET (..),

    -- ** Variables
    Var (..),
    VarLabel (..),

    -- * Renaming
    Ren,
    ren,
    varSuc,
    extRen,

    -- * Substitution
    Sub,
    sub,
    sub0,
    extSub,
    idSub,

    -- * Handling recursive sessions
    contractive,
    unroll,

    -- * Supporting definitions
    PhantomIdx (..),
  )
where

import BSession.Nat
import Data.Functor.Const
import Data.Hashable
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter

-- |  A `VarLabel` is the user facing name of a variable.
newtype VarLabel = VarLabel T.Text
  deriving newtype (IsString, Pretty)

-- | Because `VarLabel`s are purely visual they have no effect on equality. All
-- `VarLabel`s compare equal!
instance Eq VarLabel where
  _ == _ = True

instance Hashable VarLabel where
  hashWithSalt s _ = s

data Var n = Var !VarLabel !(Fin n)
  deriving stock (Eq, Generic)

instance Hashable (Var n)

instance Pretty (Var n) where
  pretty (Var lbl n) = pretty lbl <> "@" <> pretty n

data Dir = In | Out
  deriving stock (Eq, Generic)

instance Hashable Dir

newtype Ty = Ty T.Text
  deriving newtype (Eq, IsString, Pretty, Hashable)

type END :: Nat -> Type
data END n = END

instance Eq (END n) where
  _ == _ = True

instance Hashable (END n) where
  hashWithSalt s _ = s

instance PhantomIdx END where
  phantomIdx END = END

instance Pretty (END n) where
  pretty _ = "end"

type RET :: Nat -> Type
data RET n = RET

instance Eq (RET n) where
  _ == _ = True

instance Hashable (RET n) where
  hashWithSalt s _ = s

instance PhantomIdx RET where
  phantomIdx RET = RET

instance Pretty (RET n) where
  pretty _ = ".."

type Session :: Nat -> (Nat -> Type) -> Type
data Session n a where
  SEnd :: a n -> Session n a
  SVar :: !(Var n) -> Session n a
  SCom :: !Dir -> !Ty -> Session n a -> Session n a
  SAlt :: !Dir -> NE.NonEmpty (Session n a) -> Session n a
  SMu :: !VarLabel -> Session (S n) a -> Session n a

deriving stock instance (forall m. Eq (a m)) => Eq (Session n a)

deriving stock instance Generic (Session n a)

instance (forall m. Hashable (a m)) => Hashable (Session n a)

instance (forall m. Pretty (a m)) => Pretty (Session n a) where
  pretty = \case
    SEnd a -> pretty a
    SVar v -> pretty v
    SCom x t s -> (case x of In -> "?"; Out -> "!") <> pretty t <+> dot <+> pretty s
    SAlt x ss -> (case x of In -> "&"; Out -> "+") <> encloseSep "{ " " }" " ; " (pretty <$> NE.toList ss)
    SMu v s -> "rec " <> pretty v <> dot <+> pretty s

instance (forall m. Pretty (a m)) => Show (Session n a) where
  showsPrec p = showParen (p > 0) . shows . pretty

contractive :: Session n a -> Bool
contractive = go Z
  where
    go :: Nat -> Session n a -> Bool
    go !preceedingBinders = \case
      SEnd _ -> True
      SVar (Var _ n) -> finToNat n >= preceedingBinders
      SCom _ _ s -> contractive s
      SAlt _ ss -> all contractive ss
      SMu _ s -> go (S preceedingBinders) s

type PhantomIdx :: forall k. (k -> Type) -> Constraint
class PhantomIdx f where
  phantomIdx :: f a -> f b

instance PhantomIdx (Const f) where
  phantomIdx (Const x) = (Const x)

type Ren m n = Var m -> Var n

extRen :: Ren m n -> Ren (S m) (S n)
extRen _ (Var v FZ) = Var v FZ
extRen r (Var v (FS m)) = varSuc (r (Var v m))

varSuc :: Ren n (S n)
varSuc (Var v n) = Var v (FS n)

ren :: (PhantomIdx a) => Ren m n -> Session m a -> Session n a
ren r = sub (SVar . r)

type Sub a m n = Var m -> Session n a

idSub :: Sub a n n
idSub = SVar

extSub :: (PhantomIdx a) => Sub a m n -> Sub a (S m) (S n)
extSub _ (Var v FZ) = SVar (Var v FZ)
extSub s (Var v (FS m)) = ren varSuc $ s $ Var v m

sub0 :: Session n a -> Sub a (S n) n
sub0 s (Var _ FZ) = s
sub0 _ (Var v (FS n)) = SVar $ Var v n

sub :: (PhantomIdx a) => Sub a m n -> Session m a -> Session n a
sub sb = \case
  SEnd a -> SEnd $ phantomIdx a
  SVar v -> sb v
  SCom x t s -> SCom x t (sub sb s)
  SAlt x ss -> SAlt x (sub sb <$> ss)
  SMu v s -> SMu v (sub (extSub sb) s)

unroll :: (PhantomIdx a) => VarLabel -> Session (S n) a -> Session n a
unroll lbl s = sub (sub0 (SMu lbl s)) s
