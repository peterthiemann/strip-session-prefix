{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BSession.Parse
  ( parseSession,
    ParseError,
    ParseEnd,
  )
where

import BSession.Nat
import BSession.Syntax
import Control.Monad
import Data.Char qualified as C
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL

type Parser = Parsec Void T.Text

type ParseError = ParseErrorBundle T.Text Void

pSym :: String -> Parser T.Text
pSym = PL.symbol (hidden P.space) . T.pack

pLex :: Parser a -> Parser a
pLex = PL.lexeme (hidden P.space)

type ParseEnd :: (Nat -> Type) -> Constraint
class ParseEnd a where
  endParser :: Parser (a n)

instance ParseEnd END where
  endParser = END <$ pIdent KWEnd

instance ParseEnd RET where
  endParser = RET <$ pSym ".."

parseSession :: (ParseEnd a) => String -> T.Text -> Either ParseError (Session Z a)
parseSession = runParser (hidden P.space *> pSession HM.empty <* eof)

pSession :: (ParseEnd a) => HM.HashMap T.Text (Fin n) -> Parser (Session n a)
pSession vars = label "session type" do
  pCom <|> pAlt <|> pStop <|> pMu
  where
    pCom = label "communication" do
      SCom
        <$> ((In <$ pSym "?") <|> (Out <$ pSym "!"))
        <*> pIdentMap \case Ident s -> Right (Ty s); _ -> Left [Ident ""]
        <* pSym "."
        <*> pSession vars
    pAlt = label "choice" do
      SAlt
        <$> ((In <$ pSym "&") <|> (Out <$ pSym "+"))
        <*> between (pSym "{") (pSym "}") (NE.fromList <$> sepBy1 (pSession vars) (pSym ";"))
    pStop =
      (SEnd <$> endParser) <|> pIdentMapM \case
        Ident s | Just idx <- HM.lookup s vars -> pure $ Right $ SVar $ Var (VarLabel s) idx
        Ident s | otherwise -> fail $ "unbound variable ‘" ++ T.unpack s ++ "’"
        _ -> pure $ Left [Ident ""]
    pMu = do
      _ <- pIdent KWRec
      var <- pIdentMap \case Ident s -> Right s; _ -> Left [Ident ""]
      _ <- pSym "."
      let vars' = HM.insert var FZ $ HM.map FS vars
      SMu (VarLabel var) <$> pSession vars'

data LexIdent = KWRec | KWEnd | Ident !T.Text
  deriving stock (Eq)

pIdentAny :: Parser LexIdent
pIdentAny = label "identifier" $ pLex $ try do
  loc <- getOffset
  s <- takeWhile1P Nothing \c -> c == '_' || C.isAlphaNum c
  when (C.isNumber (T.head s)) $ region (setErrorOffset loc) do
    unexpected $ Tokens $ NE.fromList $ T.unpack s
  pure $ case s of
    "rec" -> KWRec
    "end" -> KWEnd
    _ -> Ident s

pIdentMapM :: (LexIdent -> Parser (Either [LexIdent] a)) -> Parser a
pIdentMapM f = try do
  loc <- getOffset
  pIdentAny >>= \lexed -> do
    let toLbl = \case
          KWRec -> Tokens $ NE.fromList "rec"
          KWEnd -> Tokens $ NE.fromList "end"
          Ident _ -> Label $ NE.fromList "identifier"
    f lexed >>= \case
      Left expected -> region (setErrorOffset loc) do
        failure (Just (toLbl lexed)) $ Set.fromList $ toLbl <$> expected
      Right a -> pure a

pIdentMap :: (LexIdent -> Either [LexIdent] a) -> Parser a
pIdentMap f = pIdentMapM (pure . f)

pIdent :: LexIdent -> Parser LexIdent
pIdent l = pIdentMap \lexed ->
  if l == lexed
    then Right lexed
    else Left [l]
