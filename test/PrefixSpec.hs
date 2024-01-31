{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PrefixSpec (spec) where

import BSession.Nat
import BSession.Parse
import BSession.Prefix
import BSession.Syntax
import Control.Applicative
import Control.Category ((>>>))
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec
import Text.Megaparsec qualified as M

spec :: Spec
spec = do
  it "recognizes basic linear prefixes" $
    ( "!A.?B.end",
      "!A.ret"
    )
      `shouldGive` "?B.end"

  it "can take a finite prefix of infinity" $
    ( "rec X. !Int.X",
      "!Int.!Int.ret"
    )
      `shouldGive` "rec X. !Int.X"

  it "can unify multiple branches" $
    ( "!A. +{ !B1.!C.end ; !B2.!C.end }",
      "!A. +{ !B1.ret ; !B2.ret }"
    )
      `shouldGive` "!C.end"

{-
  it "can take a closed loop of infinity" $
    PrefixSpec
      { full = "rec X. !Int. +{ X ; ?Int.end }",
        prefix = "rec X. !Int. +{ X ; ret }",
        cont = ""
      }
-}

infix 2 `shouldGive`

shouldGive :: (HasCallStack) => (T.Text, T.Text) -> T.Text -> Expectation
shouldGive (full, pfx) expected = do
  sfull <- shouldParse "«full»" full
  spfx <- shouldParse "«prefix»" pfx
  sexp <- shouldParse "«expected»" expected
  (sfull `stripPrefix` spfx) `shouldReturn` sexp

shouldParse :: (HasCallStack) => String -> T.Text -> IO (CSession Z)
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
