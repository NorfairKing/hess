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
    <*> strOption
        ( value "mail.txt"
        <> short 'o'
        <> long "output"
        <> metavar "FILE"
        <> help "Where to output the email addresses")
    <*> strOption
        ( value "/tmp/url.txt"
        <> short 'u'
        <> long "visited-log"
        <> metavar "FILE"
        <> help "Where to log visited URL's")

parseArgs :: IO (String, Args)
parseArgs = execParser opts
  where
    opts = info (helper <*> fullParser)
      ( fullDesc
     <> progDesc "Web spider with the objective to find email addresses."
     <> header "HEAS: Haskell Email Address Spider" )

