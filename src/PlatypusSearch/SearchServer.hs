{-# LANGUAGE OverloadedStrings #-}
module PlatypusSearch.SearchServer where
--  ( server
--  , runCommandWithInputAndSendResponse
--  , runCommandWithInput
--  , runSearchAndSendResponse
--  ) where

import Network.Wai
import Network.Wai.Parse
import System.Process
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Control.Concurrent.Async (mapConcurrently)
import Network.HTTP.Types (status400, status404, status200)
import Data.Text.Encoding (decodeUtf8)
import Data.List
import qualified Data.Text as Text
import System.Directory
import System.IO

server
  :: [String]
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
server repos req respond = do
  (params', _files) <- parseRequestBodyEx
    (setMaxRequestFileSize (1024*1024) defaultParseRequestBodyOptions)
    lbsBackEnd
    req
  let
    allParams = map toText (queryString req ++ map (fmap Just) params')
  response <- case pathInfo req of

    ["search"] -> case allParams of
      [("q", Just query)] -> do
        print query
        runSearchAndSendResponse repos query
      _ -> return wrongQuery

    ["source"] -> case allParams of
      [("file", Just fileName)] -> do
        -- Make sure you can fetch only from predefined directory.
        fileNameStr <- makeAbsolute =<< canonicalizePath (Text.unpack fileName)
        if any (`isPrefixOf` fileNameStr) repos
          then runCommandWithInputAndSendResponse (getFileHtmlCommand fileNameStr) Nothing
          else return wrongQuery
      _ -> return wrongQuery

    _ -> return notFound

  respond response
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)

getGitFileList :: String -> IO [LBS.ByteString]
getGitFileList repository = do
  outputStr <- runCommandWithInput (mconcat ["cd ", repository, " ; git ls-files"]) Nothing
  return (map (repository_prefix <>) (filter (not . LBS.null) (LBSC.split '\n' outputStr)))
  where
  repository_prefix :: LBS.ByteString
  repository_prefix = LBS.fromString (repository <> "/")

getFileListForAllRepositories :: [String] -> IO [LBS.ByteString]
getFileListForAllRepositories repositories = mconcat <$> mapConcurrently getGitFileList repositories

runCommandWithInput :: String -> Maybe LBS.ByteString -> IO LBS.ByteString
runCommandWithInput command input = do
  (stdin', output, stderr', process) <- runInteractiveCommand command
  case input of
    Nothing -> pure ()
    Just input' -> LBS.hPut stdin' input'
  outputStr <- LBS.fromStrict . LBS.toStrict <$> LBS.hGetContents output
  putStrLn "Have output"
  hClose stdin'
  hClose stderr'
  putStrLn "Closed"
  -- _ <- waitForProcess process
  putStrLn "Waited for process"
  pure $ LBS.fromStrict $ LBS.toStrict outputStr

runSearchAndSendResponse :: [String] -> Text.Text -> IO Response
runSearchAndSendResponse repos query = do
  files <- getFileListForAllRepositories repos
  runCommandWithInputAndSendResponse searchCmd (Just (LBS.intercalate "\n" files))
  where
  searchCmd = parallelSeachAndProcessCommand (getSearchCommand query)



runCommandWithInputAndSendResponse :: String -> Maybe LBS.ByteString -> IO Response
runCommandWithInputAndSendResponse command input = do
  outputStr <- runCommandWithInput command input
  return $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    outputStr

getSearchCommand :: Text.Text -> String
getSearchCommand query = mconcat
  [ "grep --color=always -n "
  , " '", Text.unpack $ Text.replace  "'" "'\"'\"'"  query, "' "
  ]

parallelSeachAndProcessCommand :: String -> String
parallelSeachAndProcessCommand command = mconcat
  [ "parallel -N 100 "
  , command, " {} "
  , "| head -c 10000000 "
  , "| ansi2html -w "
  , "| sed -e 's/\\(<b[^>]*>\\)\\([^<]*\\)\\(<\\/b><b[^>]*>:<\\/b><b[^>]*>\\([0-9]*\\)\\)/\\1<a class=\"nostyle\" href=\"source?file=\\2#line\\4\">\\2<\\/a>\\3/;s/<\\/style>/a.nostyle:link {text-decoration: inherit;color: inherit;cursor: auto;}<\\/style>/' "
  ]


getFileHtmlCommand :: String -> String
getFileHtmlCommand file = mconcat
  [ "bat --paging never --wrap never --style numbers,changes,header --color=always --theme 'Monokai Extended Light' "
  , file, " "
  , "| ansi2html -w "
  , "| sed -e 's/\\(<b[^>]*> *\\)\\([0-9][0-9]*\\)\\(<\\/b>\\)/\\1<a id=\"line\\2\"\\/>\\2\\3/' "
  , "| sed -e 's/HIW {color: #ffffff}/HIW {color: #000000}/' "
  ]

wrongQuery :: Response
wrongQuery = responseBuilder
  status400
  [ ("Content-Type", "text/plain")
  ]
  "Query does not make sense."

notFound :: Response
notFound = responseBuilder
  status404
  [ ("Content-Type", "text/plain")
  ]
  "Page not found."
