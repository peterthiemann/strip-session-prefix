{-# LANGUAGE OverloadedStrings #-}

module BSession.Prefix (stripPrefix) where

import BSession.Syntax
import Data.Foldable
import Data.HashSet qualified as HS
import Data.Maybe
import Prettyprinter
import System.Exit qualified as Exit

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

stripPrefix :: Session0 -> Session0 -> IO Session0
stripPrefix full pfx =
  go HS.empty full pfx
    >>= maybe (notAPrefix full pfx "no remainder") pure

go :: HS.HashSet (Session0, Session0) -> Session0 -> Session0 -> IO (Maybe Session0)
go !seen full pfx
  | (full, pfx) `HS.member` seen = notAPrefix full pfx "recursive"
  | otherwise = go' ((full, pfx) `HS.insert` seen) full pfx

go' :: HS.HashSet (Session0, Session0) -> Session0 -> Session0 -> IO (Maybe Session0)
go' _ s SRet = pure (Just s) -- TODO: think about behaviour if `s == SRet`
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

notAPrefix :: Session0 -> Session0 -> Doc ann -> IO a
notAPrefix full pfx reason =
  Exit.die . show . vcat $
    [ pretty pfx,
      indent 2 "is not a prefix of",
      pretty full,
      "",
      "Reason:" <+> reason
    ]
