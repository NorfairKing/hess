module Parser where

import           Types

import           Options.Applicative


data Args = Args {
        _arg_startingUrl :: String
    } deriving (Show, Eq)

makeLenses ''Args

argParser :: Parser Args
argParser = Args
    <$> strArgument
      ( metavar "SEED"
     <> help "The url to start the spider from" )

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (helper <*> argParser)
      ( fullDesc
     <> progDesc "Web spider with the objective to find email addresses."
     <> header "HEAS: Haskell Email Address Spider" )
