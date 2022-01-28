module Options where

import           Options.Applicative (Parser, ParserInfo, fullDesc, help, helper
                                    , info, long, metavar, progDesc, short
                                    , showDefault, strOption, value, (<**>))

data Options = Options { sourceFile :: String, outputFile :: String }
  deriving (Eq, Show)

optionsParser :: ParserInfo Options
optionsParser =
  info (parser <**> helper) (fullDesc <> progDesc "Transpile a DotScript code")
  where
    parser :: Parser Options
    parser = Options
      <$> strOption
        (long "source"
         <> short 's'
         <> metavar "SOURCE"
         <> help "Source file"
         <> value "main.dts"
         <> showDefault)
      <*> strOption
        (long "output"
         <> short 'o'
         <> metavar "OUTPUT"
         <> help "Output file"
         <> value "main.js"
         <> showDefault)
