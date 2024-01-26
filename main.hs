{-# LANGUAGE BlockArguments #-}

import BSession.Parse qualified as Parse
import Control.Monad
import Data.Foldable
import Data.Text qualified as T
import Prettyprinter
import System.Environment
import Text.Megaparsec qualified as P

main :: IO ()
main = do
  args <- getArgs
  for_ (zip [1 :: Int ..] args) \(i, arg) -> do
    when (i > 1) $ putChar '\n'
    putStrLn $ "\ESC[1m[== " ++ show i ++ " ==]\ESC[0m"
    case P.runParser Parse.session "" (T.pack arg) of
      Left e -> do
        putStr $ P.errorBundlePretty e
      Right s -> do
        print $ pretty s
