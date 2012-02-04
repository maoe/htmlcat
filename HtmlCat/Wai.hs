{-# LANGUAGE OverloadedStrings #-}
module HtmlCat.Wai (feedStdIn, runHtmlCat) where
import Control.Concurrent (Chan, writeChan, forkIO)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Prelude hiding (lines)
import System.IO (stdin)
import qualified Data.Text as T

import Data.Conduit (($$), ($=), ResourceIO, Source, Sink, SinkResult(..), Conduit, runResourceT, sinkIO)
import Data.Conduit.Binary (sourceHandle)
import Data.Conduit.Text (decode, utf8)
import Network.HTTP.Types (headerContentType, statusOK, statusNotFound)
import Network.Wai (Application, Request(..), Response(..), responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceApp)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Conduit.List as CL

import HtmlCat.Html (html)

feedStdIn :: Chan ServerEvent -> IO ()
feedStdIn chan = void . forkIO . runResourceT $
  sourceStdIn $= lines $= textsToEventSource $$ sinkChan chan

runHtmlCat :: Chan ServerEvent -> String -> Int -> IO ()
runHtmlCat chan host port =
  runSettings (defaultSettings { settingsHost = host
                               , settingsPort = port })
              (app chan)

app :: Chan ServerEvent -> Application
app chan req =
  case pathInfo req of
    []         -> appTop req
    ["stream"] -> appStream chan req
    _          -> app404 req

appTop :: Application
appTop _ = return $
  ResponseBuilder statusOK
                  [headerContentType "text/html; charset=utf-8"]
                  (renderHtmlBuilder html)

appStream :: Chan ServerEvent -> Application
appStream = eventSourceApp

app404 :: Application
app404 _ = return $ responseLBS statusNotFound [] "Not found"

sourceStdIn :: ResourceIO m => Source m Text
sourceStdIn = sourceHandle stdin $= decode utf8

lines :: Monad m => Conduit Text m [Text]
lines = CL.map T.lines

textsToEventSource :: Monad m => Conduit [Text] m ServerEvent
textsToEventSource = CL.map f
  where
    f texts = ServerEvent { eventName = Nothing
                          , eventId   = Nothing
                          , eventData = map B.fromText texts }

sinkChan :: ResourceIO m => Chan a -> Sink a m ()
sinkChan chan = sinkIO noop (const noop) push return
  where
    noop = return ()
    push _ a = do
      liftIO $ writeChan chan a
      return Processing
