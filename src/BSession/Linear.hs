{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BSession.Linear where

import BSession.Nat
import BSession.Syntax
import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Prettyprinter
import System.Exit

linearSessions :: CSession n -> NE.NonEmpty (LSession n)
linearSessions = \case
  SEnd -> NE.singleton SEnd
  SRet -> NE.singleton SRet
  SVar v -> NE.singleton (SVar v)
  SMu v s -> SMu v <$> linearSessions s
  SCom x t s -> SCom x t <$> linearSessions s
  SAlt x ss -> unpackBranches ss >>= fmap (SAlt x) . traverse linearSessions

recombineChoices :: NE.NonEmpty (LSession n) -> IO (CSession n)
recombineChoices (s0 :| ss) = case s0 of
  SEnd | all (SEnd ==) ss -> pure SEnd
  SRet | all (SRet ==) ss -> pure SRet
  SVar v | all (SVar v ==) ss -> pure (SVar v)
  SMu v s0' | Just ss' <- traverse uncons ss -> SMu v <$> recombineChoices (s0' :| ss')
    where
      uncons (SMu v' s') | v == v' = pure s'
      uncons _ = empty
  SCom x t s0' | Just ss' <- traverse uncons ss -> SCom x t <$> recombineChoices (s0' :| ss')
    where
      uncons (SCom x' t' s') | x == x' && t == t' = pure s'
      uncons _ = empty
  SAlt x b0 | Just bs' <- traverse uncons ss -> do
    let grouped = NE.groupAllWith1 branchIndex (b0 :| bs')
    let recombineGroup (LBranch i w a :| as) =
          LBranch i w <$> recombineChoices (a :| fmap branchTarget as)
    SAlt x . padLBranches voidSession <$> traverse recombineGroup grouped
    where
      uncons (SAlt x' b') | x == x' = pure b'
      uncons _ = empty
      voidSession = SMu "void" $ SVar $ Var "void" FZ
  _ ->
    die . show $
      "Can't reconcile linear sessions"
        <> line
        <> indent 3 (vcat [pretty s | s <- s0 : ss])
