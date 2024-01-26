{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

import BSession.Parse qualified as Parse
import BSession.Prefix
import BSession.Syntax
import Data.Text qualified as T
import Prettyprinter
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec qualified as P

main :: IO ()
main = do
  args <- getArgs
  (srcFull, srcPfx) <- case args of
    [full, pfx] -> pure (full, pfx)
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "usage: " ++ prog ++ " full_type prefix"
      exitFailure

  (mfull, mpfx) <-
    (,)
      <$> parse "full type" srcFull
      <* putChar '\n'
      <*> parse "prefix" srcPfx
  (full, pfx) <- maybe exitFailure pure do
    (,) <$> mfull <*> mpfx

  putChar '\n'
  printHeader "continuation"
  print . pretty =<< stripPrefix full pfx

parse :: String -> String -> IO (Maybe Session0)
parse name src = do
  printHeader name
  case P.runParser Parse.session "" (T.pack src) of
    Left e -> do
      putStr $ P.errorBundlePretty e
      pure Nothing
    Right s -> do
      print $ pretty s
      pure (Just s)

printHeader :: String -> IO ()
printHeader s = putStrLn $ "\ESC[1m[== " ++ s ++ " ==]\ESC[0m"
