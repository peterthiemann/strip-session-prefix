{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module BSession.Prefix where

import BSession.Nat
import BSession.Syntax
import Data.Foldable
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Semialign
import Data.These

{-
The current algorithm is wrong. Take for example the type

  rec X. !Int. +{ X; ?Int.end }

a valid prefix would be

  rec X. !Int. +{ X; ret }

but this is refused by the current version. Once we get to the choice the types
look like

  +{ rec X. !Int +{ X; ?Int.end }; ?Int.end }

and

  +{ rec X. !Int +{ X; ret }; ret }

When moving into the left branch we discover that we've seen this instance
before and conclude with a recusion errror.

Possible alternative algorithm: look at the paths through the session type. On
second thought this sounds similar to the session type -> regular language ->
automata construction idea.
-}

data Error
  = ErrorNotAPrefix
  | ErrorNoCont
  | ErrorContConflict (Session Z END) (Session Z END)
  deriving stock (Eq, Show)

stripPrefix :: Session Z END -> Session Z RET -> Either Error (Session Z END)
stripPrefix s0 p0 = go HS.empty s0 p0 >>= maybe (Left ErrorNoCont) Right
  where
    go :: HS.HashSet (Session Z END, Session Z RET) -> Session Z END -> Session Z RET -> Either Error (Maybe (Session Z END))
    go !seen s p
      | (s, p) `HS.member` seen = Right Nothing
      | otherwise = go' (HS.insert (s, p) seen) s p

    go' :: HS.HashSet (Session Z END, Session Z RET) -> Session Z END -> Session Z RET -> Either Error (Maybe (Session Z END))
    go' _ s (SEnd _) = pure $ Just $ s
    go' c (SMu v s) s' = go c (unroll v s) s'
    go' c s (SMu v s') = go c s (unroll v s')
    go' c (SCom xs ts s) (SCom xp tp p) | xs == xp && ts == tp = go c s p
    go' c (SAlt xs bbs) (SAlt xp bbp)
      | xs == xp,
        Just bb <- zipSameLength bbs bbp = do
          rests <- traverse (uncurry (go c)) bb
          case catMaybes (toList rests) of
            [] -> pure Nothing
            r : rs
              | r' : _ <- filter (r /=) rs -> Left (ErrorContConflict r r')
              | otherwise -> pure $ Just r
    go' _ _ _ = Left ErrorNotAPrefix

zipSameLength :: (Semialign f, Traversable f) => f a -> f b -> Maybe (f (a, b))
zipSameLength fa fb = sequenceA $ alignWith (\case These a b -> Just (a, b); _ -> Nothing) fa fb

{-
type VarMap :: Nat -> (Nat -> Type) -> Type
data VarMap n f where
  VMNil :: VarMap Z f
  VMCons :: f (S n) -> VarMap n f -> VarMap (S n) f

type VarCache :: (Nat -> Type) -> Type -> Nat -> Type
data VarCache a t n = VarCache
  { varExpansion :: Session n a,
    varSeen :: [t]
  }

closeVarMap :: (PhantomIdx a) => (forall m. f m -> Session m a) -> VarMap n f -> Sub a n Z
closeVarMap _ VMNil = idSub
closeVarMap g (VMCons a m) = \case
  Var v FZ -> closeSession g m (sub (sub0 (SMu v (g a))) (g a))
  Var v (FS n) -> closeVarMap g m (Var v n)

closeSession :: (PhantomIdx a) => (forall m. f m -> Session m a) -> VarMap n f -> Session n a -> Session Z a
closeSession g = sub . closeVarMap g

{-
lookupVar :: (PhantomIdx a) => Var n -> VarMap n a -> Session n a
lookupVar (Var _ FZ) (VMCons s _) = s
lookupVar (Var v (FS n)) (VMCons _ m) = ren varSuc $ lookupVar (Var v n) m
-}

stripPrefix :: Session Z END -> Session Z RET -> Result Z
stripPrefix s0 p0 = go VMNil VMNil s0 p0 >>= maybe noRemainder Right
  where
    go :: VarMap m (VarCache END (Session n RET)) -> VarMap n (VarCache RET ()) -> Session m END -> Session n RET -> MResult Z
    go mvars _ s (SEnd _) = Right $ Just $ closeSession varExpansion mvars s
    go svars pvars s (SMu _ p) = go svars (VMCons (VarCache p []) _) s p
    go svars pvars (SMu _ s) p = go (VMCons (VarCache s []) svars) pvars s p
    go svars pvars (SCom xs ts s) (SCom xp tp p)
      | xs == xp && ts == tp = first (SCom xs ts) $ go svars pvars s p
    go mvars nvars s s' = Left . SEnd $ do
      Error
        { errorFull = closeSession varExpansion mvars s,
          errorPrefix = closeSession varExpansion nvars s',
          errorReason = ErrorIncompatible
        }

    noRemainder = Left . SEnd $ do
      Error
        { errorFull = s0,
          errorPrefix = p0,
          errorReason = ErrorNoRemainder
        }
-}

{-
stripPrefix :: S0 -> S0 -> IO S0
stripPrefix full0 pfx0 =
  go HS.empty full0 pfx0
    >>= maybe (notAPrefix full0 pfx0 "no remainder") pure
  where
    go :: HS.HashSet (S0, S0) -> S0 -> S0 -> IO (Maybe S0)
    go !seen full pfx
      | (full, pfx) `HS.member` seen = notAPrefix full pfx "recursive"
      | otherwise = go' ((full, pfx) `HS.insert` seen) full pfx

    go' :: HS.HashSet (S0, S0) -> S0 -> S0 -> IO (Maybe S0)
    go' _ s (SEnd _) = pure (Just s) -- TODO: think about behaviour if `s == SRet`
    go' _ s1 s2 | s1 == s2 = pure Nothing
    go' seen (SCom x1 t1 s1) (SCom x2 t2 s2) | x1 == x2 && t1 == t2 = go seen s1 s2
    go' seen full@(SAlt x1 ss1) pfx@(SAlt x2 ss2) | x1 == x2 && length ss1 == length ss2 = do
      conts <- catMaybes <$> traverse (uncurry (go seen)) (zip (toList ss1) (toList ss2))
      case conts of
        [] -> notAPrefix full pfx "no remainder"
        (s : ss) | all (s ==) ss -> pure $ Just s
        ss -> notAPrefix full pfx $ "incompatible remainders" <> line <> indent 2 (vcat ["*" <+> pretty s | s <- ss])
    go' seen (SMu v s) s' = go seen (unroll v s) s'
    go' seen s (SMu v s') = go seen s (unroll v s')
    go' _ full pfx = notAPrefix full pfx "incompatible structure"

notAPrefix ::
  (forall m. Pretty (a m), forall m. Pretty (b m)) =>
  Session n a ->
  Session n b ->
  Doc ann ->
  IO x
notAPrefix full pfx reason =
  Exit.die . show . vcat $
    [ pretty pfx,
      indent 2 "is not a prefix of",
      pretty full,
      "",
      "Reason:" <+> reason
    ]
-}

{-
stripPrefix' :: CSession Z -> CSession Z -> IO (CSession Z)
stripPrefix' full pfx =
  let fullPaths = linearSessions full
   in undefined

stripLinPrefix :: LSession n -> CSession n -> IO (LSession n)
stripLinPrefix = go
  where
    go SRet SRet = fail "TODO: handle ret % ret case"
    go s SRet = pure s
    go a@SEnd b@SEnd = notAPrefix a b "no continuation left"
    go a@(SVar v) b@(SVar v') | v == v' = notAPrefix a b "no continuation left"
    go (SCom x t full) (SCom x' t' pfx) | x == x' && t == t' = go full pfx
    go (SAlt x b) (SAlt x' bs) | x == x' && branchWidth b == branchWidth' bs = go (branchTarget b) (toList bs `genericIndex` branchIndex b)
    go (SMu v full) pfx = undefined
    go full (SMu v pfx) = undefined
    go full pfx = notAPrefix full pfx "incompatible structure"
    -}
