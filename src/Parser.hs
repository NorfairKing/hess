module Parser where

import           Monad

import           Options.Applicative



fullParser :: Parser (String, Args)
fullParser = (,)
    <$> strArgument
        ( metavar "SEED"
        <> help "The url to start the spider from" )
    <*> argParser

argParser :: Parser Args
argParser = Args
    <$> option auto
        ( value 1
        <> short 'f'
        <> long "fetchers"
        <> metavar "INT"
        <> help "The number of fetchers to use concurrently")

parseArgs :: IO (String, Args)
parseArgs = execParser opts
  where
    opts = info (helper <*> fullParser)
      ( fullDesc
     <> progDesc "Web spider with the objective to find email addresses."
     <> header "HEAS: Haskell Email Address Spider" )

