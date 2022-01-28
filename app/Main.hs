module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options (optionsParser, outputFile, sourceFile)
import           Phosphor.Parser (parser)
import           Options.Applicative (execParser)
import           Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  options <- execParser optionsParser
  source <- TIO.readFile (sourceFile options)
  let res = parse parser (sourceFile options) source
  case res of
    Left err  -> putStrLn $ errorBundlePretty err
    Right ast -> print ast
  pure ()
