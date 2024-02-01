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
import Data.Hashable
import Data.Maybe
import Data.Semialign
import Data.These

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
              | r' : _ <- filter (not . isEqual r) rs -> Left (ErrorContConflict r r')
              | otherwise -> pure $ Just r
    go' _ _ _ = Left ErrorNotAPrefix

zipSameLength :: (Semialign f, Traversable f) => f a -> f b -> Maybe (f (a, b))
zipSameLength fa fb = sequenceA $ alignWith (\case These a b -> Just (a, b); _ -> Nothing) fa fb

isEqual ::
  forall a.
  (PhantomIdx a, forall n. Hashable (a n)) =>
  Session Z a ->
  Session Z a ->
  Bool
isEqual = go HS.empty
  where
    go :: HS.HashSet (Session Z a, Session Z a) -> Session Z a -> Session Z a -> Bool
    go !seen a b
      | (a, b) `HS.member` seen = True
      | otherwise = go' (HS.insert (a, b) seen) a b

    go' :: HS.HashSet (Session Z a, Session Z a) -> Session Z a -> Session Z a -> Bool
    go' _ (SEnd x) (SEnd y) = x == y
    go' c (SCom x t a) (SCom y u b) = x == y && t == u && go c a b
    go' c (SAlt x b1) (SAlt y b2) = x == y && fromMaybe False (all (uncurry (go c)) <$> zipSameLength b1 b2)
    go' c (SMu v a) b = go c (unroll v a) b
    go' c a (SMu v b) = go c a (unroll v b)
    go' _ _ _ = False
