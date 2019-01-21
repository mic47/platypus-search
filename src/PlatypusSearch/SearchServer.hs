module PlatypusSearch.SearchServer 
  ( server
  ) where

import Network.Wai
import Network.Wai.Parse
import System.Process hiding (runCommand)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status400, status404, status200)
import Data.Text.Encoding (decodeUtf8)
import Data.List
import qualified Data.Text as Text
import System.Directory
import System.IO
import Data.Text.Internal.Unsafe.Char (unsafeChr8)

import Debug.Trace

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
    allParams = sort $ map toText (queryString req ++ map (fmap Just) params')
  response <- case pathInfo req of

    ["search"] -> case allParams of 
      [("q", Just query)] -> do
        putStrLn (show query)
        runCommandAndSendResponse (traceShowId $ getSearchCommand repos query)
      _ -> return wrongQuery

    ["compare"] -> case allParams of
      [("q1", Just query1), ("q2", Just query2)] -> do
        putStrLn (show query1)
        putStrLn (show query2)
        putStrLn ("FFFUUUUUU")
        commFiles <- lines . map unsafeChr8 . LBS.unpack . traceShowId <$> runCommand (traceShowId $ getCommCommand repos query1 query2)
        putStrLn $ show commFiles
        if null commFiles then sendResponse mempty else runCommandAndSendResponse $ traceShowId $ getSearchCommand commFiles $ combineGrepQueries query1 query2
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
runCommandAndSendResponse s = sendResponse =<< runCommand s

runCommand :: String -> IO LBS.ByteString
runCommand command = do
  (h1, output, h2, process) <- runInteractiveCommand command
  outputStr <- LBS.hGetContents output
  putStrLn "Have output"
  hClose h1
  hClose h2
  putStrLn "Closed"
  -- _ <- waitForProcess process
  putStrLn "Waited for process"
  return outputStr

sendResponse :: LBS.ByteString -> IO Response
sendResponse outputStr = do
  return $ responseLBS
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    outputStr

getCommCommand :: [String] -> Text.Text -> Text.Text -> String
getCommCommand repos query1 query2 = 
  let 
    cmd1 = getGrepCommand "never" repos query1 ++ " -l"
    cmd2 = getGrepCommand "never" repos query2 ++ " -l"
  in mconcat
      [ "a=$(tempfile); b=$(tempfile); "
      , cmd1, " | sort > $a ; "
      , cmd2, " | sort > $b ; "
      , "comm -12 $a $b"
      ]

combineGrepQueries :: Text.Text -> Text.Text -> Text.Text
combineGrepQueries query1 query2 = mconcat
  [ "\\(", query1
  , "\\|", query2
  , "\\)"
  ]

getGrepCommand :: String -> [String] -> Text.Text -> String
getGrepCommand color repos query = mconcat
  [ "grep --color=", color, " -rn "
  , grepFlags
  , " '", Text.unpack $ Text.replace  "'" "'\"'\"'"  query, "' "
  , unwords repos
  ]


getSearchCommand :: [String] -> Text.Text -> String
getSearchCommand repos query = mconcat
  [ getGrepCommand "always" repos query, " "
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
  , "--exclude-dir='.stack-work'"
  , "--exclude='*.swp'"
  , "--exclude='*.swo'"
  , "--exclude='.generated.ctags'"
  , "--exclude='.generated.sctags'"
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
