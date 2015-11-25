module Parser where

import           Monad
import           Types

import           Data.Maybe                (fromJust)

import           Options.Applicative
import           Options.Applicative.Types (ReadM (..), readerAsk, readerError)



fullParser :: Parser Args
fullParser = argParser

uriReadM :: ReadM URI
uriReadM = do
    s <- readerAsk
    case parseURI s of
        Nothing -> readerError "Not a valid seed URI"
        Just uri -> return uri

argParser :: Parser Args
argParser = Args
    <$> argument uriReadM
        ( metavar "SEED"
        <> help "The url to start the spider from" )
    <*> option auto
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
    <*> switch
        ( short 's'
        <> long "stay-within-domain"
        <> help "Stay within the seed URL's domain")

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (helper <*> fullParser)
      ( fullDesc
     <> progDesc "Web spider with the objective to find email addresses."
     <> header "HEAS: Haskell Email Address Spider" )

