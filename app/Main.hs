module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options (optionsParser, outputFile, sourceFile)
import           Phosphor.Parser (parser)
import           Options.Applicative (execParser)
import           Text.Megaparsec (errorBundlePretty, parse)
import           Phosphor.Transpiler (transpile)
import           Phosphor.TypeChecker (typeCheck, toErrorBundle)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  options <- execParser optionsParser
  source <- TIO.readFile (sourceFile options)
  let res = parse parser (sourceFile options) source
  case res of
    Left err  -> putStrLn $ errorBundlePretty err
    Right ast -> do
      case typeCheck ast of
        Nothing  -> TIO.writeFile (outputFile options) $ transpile ast
        Just err -> putStrLn $ errorBundlePretty $ toErrorBundle source err
  pure ()
