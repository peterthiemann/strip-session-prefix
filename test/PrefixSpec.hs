{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PrefixSpec (spec) where

import BSession.Nat
import BSession.Parse
import BSession.Prefix
import BSession.Syntax
import Control.Applicative
import Control.Category ((>>>))
import Data.Hashable
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec
import Text.Megaparsec qualified as M

spec :: Spec
spec = do
  it "recognizes basic linear prefixes" do
    ("!A.?B.end", "!A...")
      `shouldGive` "?B.end"

  it "refutes direction mismatches" do
    ("!A.end", "?A...")
      `shouldFail` ErrorNotAPrefix
    ("+{ end }", "&{ .. }")
      `shouldFail` ErrorNotAPrefix

  it "refutes type mismatches" do
    ("!A.end", "!B...")
      `shouldFail` ErrorNotAPrefix

  it "can take a finite prefix of infinity" $
    ( "rec X. !Int.X",
      "!Int.!Int..."
    )
      `shouldGive` "rec X. !Int.X"

  it "can unify multiple branches" $
    ( "!A. +{ !B1.!C.end ; !B2.!C.end }",
      "!A. +{ !B1... ; !B2... }"
    )
      `shouldGive` "!C.end"

  it "normalizes branches during unification" $
    ( "+{ !A. rec X. !C. end ; !B. !C. end }",
      "+{ !A... ; !B... }"
    )
      `shouldGive` "!C.end"

  it "can take a closed loop out of infinity" $
    ( "rec X. !Int. +{ X ; ?Int.end }",
      "rec X. !Int. +{ X ; .. }"
    )
      `shouldGive` "?Int.end"

infix 2 `shouldGive`

newtype Semantic a = Semantic (Session Z a)

deriving newtype instance (forall n. Pretty (a n)) => Show (Semantic a)

instance (PhantomIdx a, forall n. Hashable (a n)) => Eq (Semantic a) where
  Semantic a == Semantic b = isEqual a b

shouldGive :: (HasCallStack) => (T.Text, T.Text) -> T.Text -> Expectation
shouldGive (ssrc, psrc) expected = do
  s <- shouldParse "«s»" ssrc
  p <- shouldParse "«p»" psrc
  r <- shouldParse "«r»" expected
  case s `stripPrefix` p of
    Left e -> expectationFailure $ show e
    Right r' -> Semantic r' `shouldBe` Semantic r

shouldFail :: (HasCallStack) => (T.Text, T.Text) -> Error -> Expectation
shouldFail (ssrc, psrc) e = do
  s <- shouldParse "«s»" ssrc
  p <- shouldParse "«p»" psrc
  case s `stripPrefix` p of
    Left e' -> e' `shouldBe` e
    Right r -> expectationFailure $ show r

shouldParse :: (ParseEnd a, forall n. Pretty (a n), HasCallStack) => String -> T.Text -> IO (Session Z a)
shouldParse name =
  parseSession name >>> \case
    Left e -> failure (M.errorBundlePretty e)
    Right s
      | contractive s -> pure s
      | otherwise -> do
          failure . show . vcat $
            [ "session type" <+> pretty name <+> "is not contractive:",
              indent 2 $ pretty s
            ]

failure :: (HasCallStack) => String -> IO a
failure msg = expectationFailure msg *> empty
