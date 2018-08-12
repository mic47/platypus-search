{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Network.Wai.Handler.Warp
import Options.Applicative
import Data.Monoid

import PlatypusSearch.SearchServer (server)


data Options = Options
  { port :: Int
  , repositories :: [String]
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
    ( long "port"
    <> metavar "PORT"
    <> value 5047
    )
  <*> many (strOption
    ( long "repo"
    <> metavar "REPOSITORY"
    ))

main :: IO ()
main = do
  Options{..} <- execParser
    (info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Simple code search and code browsing tool."
      )
    )
  run 
    port
    (server repositories)
