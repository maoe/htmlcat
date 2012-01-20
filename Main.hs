{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module Main where
import Control.Concurrent (Chan, newChan, writeChan, forkIO)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import Data.Foldable (forM_)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network (PortID(..), listenOn, sClose)
import System.IO (stdin, stdout)
import System.Process (rawSystem)

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Conduit (($$), (=$), ($=), ResourceIO, Source, Sink, SinkResult(..), Conduit, runResourceT, sinkIO)
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.Conduit.Text (encode, decode, utf8)
import Network.HTTP.Types (headerContentType, statusOK, statusNotFound)
import Network.Wai (Application, Request(..), Response(..), responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceApp)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
import System.Console.CmdArgs
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (Html, shamlet)
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
  HtmlCat {..} <- cmdArgs htmlCat
  chan <- newChan
  port <- newPort _port
  let url = "http://" ++ _host ++ ":" ++ show port
  putStrLn url
  whenJust _exec $ \exec ->
    forkIO $ void $ rawSystem exec [url]
  runSettings (defaultSettings { settingsHost = _host
                               , settingsPort = port })
              (app chan)
  where
    whenJust = forM_

newPort :: Maybe Int -> IO Int
newPort port'm = foldr tryListening
                       (error "no available port")
                       (maybeToList port'm ++ [45192..60000])
  where
    tryListening p next = do
      r <- try . listenOn $ PortNumber (fromIntegral p)
      case r of
        Left (_ :: IOException) -> next
        Right sock -> do
          sClose sock
          return p

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
appStream chan req = do
  lift . void . forkIO . runResourceT $
    sourceStdIn $$ textToEventSource =$ sinkChan chan
  eventSourceApp chan req

app404 :: Application
app404 _ = return $ responseLBS statusNotFound [] "Not found"

sourceStdIn :: ResourceIO m => Source m Text
sourceStdIn = sourceHandle stdin $= decode utf8

sinkStdOut :: ResourceIO m => Sink Text m ()
sinkStdOut = encode utf8 =$ sinkHandle stdout

textToEventSource :: Monad m => Conduit Text m ServerEvent
textToEventSource = CL.map f
  where
    f text = ServerEvent { eventName = Nothing
                         , eventId = Nothing
                         , eventData = [fromText text] }

sinkChan :: ResourceIO m => Chan a -> Sink a m ()
sinkChan chan = sinkIO noop (const noop) push return
  where
    noop = return ()
    push _ a = do
      liftIO $ writeChan chan a
      return Processing

data HtmlCat = HtmlCat
  { _port :: Maybe Int
  , _host :: String
  , _exec :: Maybe String
  } deriving (Show, Data, Typeable)

htmlCat :: HtmlCat
htmlCat = HtmlCat
  { _port = Nothing     &= explicit &= name "port"
  , _host = "127.0.0.1" &= explicit &= name "host"
  , _exec = Nothing     &= explicit &= name "exec"
  }

html :: Html
html = [shamlet|
!!!
<html>
  <head>
    <title>htmlcat
    <script type="text/javascript">
      window.onload = function () {
        var es = new EventSource("/stream");
        es.onmessage = function(event) {
          var data = {};
          data.html = event.data;
          if (!data.html) {
            return;
          }
      
          if (window.scrollY + document.documentElement.clientHeight >= document.documentElement.scrollHeight) {
            var scrollToBottom = true;
          }
  
          var div = document.createElement('div');
          div.innerHTML = data.html + "\n";
  
          var out = document.getElementById('out');
          while (div.firstChild) {
            out.appendChild(div.firstChild);
          }
  
          document.title = data.html.replace(/<.*?>/g, '') + ' - htmlcat';
  
          if (scrollToBottom) {
            window.scrollTo(0, document.body.scrollHeight);
          }
        };
      };
  <body>
    <pre id="out">
|]
