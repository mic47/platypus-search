module PlatypusSearch.SearchServer 
  ( server
  ) where

import Network.Wai
import Network.Wai.Parse
import System.Process
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status400, status404, status200)
import Data.Text.Encoding (decodeUtf8)
import Data.List
import qualified Data.Text as Text
import System.Directory

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
      [("q", Just query)] ->
        runCommandAndSendResponse (getSearchCommand repos query)
      _ -> return wrongQuery

    ["source"] -> case allParams of 
      [("file", Just fileName)] -> do
        -- Make sure you can fetch only from predefined directory.
        fileNameStr <- makeAbsolute =<< canonicalizePath (Text.unpack fileName)
        if any (`isPrefixOf` fileNameStr) repos
          then runCommandAndSendResponse (getFileHtmlCommand fileNameStr)
          else return wrongQuery
      _ -> return wrongQuery

    _ -> return notFound

  respond response
  where
    toText (x, y) = (decodeUtf8 x, decodeUtf8 <$> y)

runCommandAndSendResponse :: String -> IO Response
runCommandAndSendResponse command = do
  (_, output, _, _) <- runInteractiveCommand command
  outputStr <- LBS.hGetContents output
  return $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    outputStr

getSearchCommand :: [String] -> Text.Text -> String
getSearchCommand repos query = mconcat
  [ "grep --color=always -rn "
  , grepFlags
  , " '", Text.unpack $ Text.replace  "'" "'\"'\"'"  query, "' "
  , unwords repos , " "
  , "| head -n 20000 "
  , "| ansi2html -w "
  , "| sed -e 's/\\(<b[^>]*>\\)\\([^<]*\\)\\(<\\/b><b[^>]*>:<\\/b><b[^>]*>\\([0-9]*\\)\\)/\\1<a class=\"nostyle\" href=\"source?file=\\2#line\\4\">\\2<\\/a>\\3/;s/<\\/style>/a.nostyle:link {text-decoration: inherit;color: inherit;cursor: auto;}<\\/style>/' "
  ]

grepFlags :: String
grepFlags = unwords
  [ "--exclude-dir=.git"
  , "--exclude-dir=target"
  , "--exclude=*.class"
  , "--exclude-dir='$global'"
  , "--exclude-dir='target'"
  , "--exclude-dir='.idea'"
  , "--exclude-dir='.ensime_cache'"
  , "--exclude-dir='node_modules'"
  , "--exclude='*.swp'"
  , "--exclude='*.swo'"
  , "--exclude='.generated.ctags'"
  , "--exclude '*.xml'"
  , "--exclude '*.json'"
  , "--exclude '*.jsonl'"
  , "--exclude '*.config'"
  , "--exclude '*.csv'"
  , "--exclude '*.txt'"
  , "--exclude '*.log'"
  , "--exclude '*.ipynb'"
  , "--exclude '*.min.js'"
  , "--exclude '*.pyc'"
  , "--exclude '*.ensime'"
  , "--exclude '*.~1~'"
  , "--exclude '*.~2~'"
  , "--exclude '*.~3~'"
  , "--exclude '*.part1'"
  , "--exclude '*.part2'"
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
