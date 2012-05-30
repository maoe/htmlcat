{-# LANGUAGE OverloadedStrings #-}
module HtmlCat.Wai (feedStdIn, runHtmlCat, newHtmlCatChan) where
import Control.Applicative ((<*))
import Control.Arrow (second)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Resource (Resource(..))
import Data.Text (Text)
import Prelude hiding (lines)
import System.IO (stdin)

import Data.Conduit (($$), ($=), ResourceIO, Source, Sink, SinkIOResult(..), Conduit, runResourceT, sinkIO)
import Data.Conduit.Binary (sourceHandle, lines)
import Data.Conduit.Text (decode, utf8)
import Network.HTTP.Types (headerContentType, statusOK, statusNotFound)
import Network.Wai (Application, Request(..), Response(..), responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceApp)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort, HostPreference(..))
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Conduit.List as CL

import HtmlCat.Html (html)
import HtmlCat.Color (parseConsoleString, defaultConsoleState, ConsoleState(..), convHtml, ColorScheme(..))
import HtmlCat.HtmlCatChan (HtmlCatChan, newHtmlCatChan, notifyClient, writeHtmlCatChan, htmlCatChan)

feedStdIn :: HtmlCatChan ServerEvent -> ColorScheme -> IO ()
feedStdIn chan cols = void . forkIO . runResourceT $
  sourceStdIn $= colorConv cols $= textsToEventSource $$ sinkChan chan

runHtmlCat :: HtmlCatChan ServerEvent -> String -> Int -> ColorScheme -> IO ()
runHtmlCat chan host port cols =
  runSettings (defaultSettings { settingsHost = Host host
                               , settingsPort = port })
              (app chan cols)

app :: HtmlCatChan ServerEvent -> ColorScheme -> Application
app chan cols req =
  case pathInfo req of
    []         -> appTop cols req
    ["stream"] -> appStream chan req
    _          -> app404 req

appTop :: ColorScheme -> Application
appTop cols _ = return $
  ResponseBuilder statusOK
                  [headerContentType "text/html; charset=utf-8"]
                  (renderHtmlBuilder $ html cols)

appStream :: HtmlCatChan ServerEvent -> Application
appStream chan req = eventSourceApp (htmlCatChan chan) req <* liftIO (notifyClient chan)

app404 :: Application
app404 _ = return $ responseLBS statusNotFound [] "Not found"

sourceStdIn :: ResourceIO m => Source m Text
sourceStdIn = sourceHandle stdin $= lines $= decode utf8

colorConv :: Resource m => ColorScheme -> Conduit Text m Text
colorConv cols = CL.concatMapAccum f defaultConsoleState { colorScheme = cols }
  where
    f text state = second return $ convHtml state $ parseConsoleString text

textsToEventSource :: Monad m => Conduit Text m ServerEvent
textsToEventSource = CL.map f
  where
    f text = ServerEvent { eventName = Nothing
                         , eventId   = Nothing
                         , eventData = [B.fromText text] }

sinkChan :: ResourceIO m => HtmlCatChan a -> Sink a m ()
sinkChan chan = sinkIO noop (const noop) push return
  where
    noop = return ()
    push _ a = do
      liftIO $ writeHtmlCatChan chan a
      return IOProcessing
