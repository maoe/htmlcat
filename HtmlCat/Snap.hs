{-# LANGUAGE OverloadedStrings #-}
module HtmlCat.Snap (feedStdIn, runHtmlCat) where
import Control.Concurrent (Chan, writeChan, forkIO)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import System.IO (stdin)
import qualified Data.ByteString.Char8 as B8

import Data.Enumerator (Iteratee, Enumeratee, ($$), ($=))
import Snap.Core
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Enumerator as E (run_)
import qualified Data.Enumerator.List as E (map, foldM, mapAccum)
import qualified Data.Enumerator.Text as E (enumHandle)

import HtmlCat.Html (html)
import HtmlCat.Color (parseConsoleString, defaultConsoleState, ConsoleState(..), convHtml, ColorScheme(..))
import Snap.EventSource (ServerEvent(..), eventSourceApp)

feedStdIn :: Chan ServerEvent -> ColorScheme -> IO ()
feedStdIn chan cols = void . forkIO $ E.run_ $
  sourceStdIn $= colorConv cols $= textsToEventSource $$ sinkChan chan

runHtmlCat :: Chan ServerEvent -> String -> Int -> ColorScheme -> IO ()
runHtmlCat chan host port cols =
  simpleHttpServe (setPort port $ setBind (B8.pack host)
                                $ defaultConfig :: Config Snap ())
                  (app chan cols)

app :: Chan ServerEvent -> ColorScheme -> Snap ()
app chan cols = route [ ("",       appTop cols)
                      , ("stream", appStream chan)
                      ]

appTop :: ColorScheme -> Snap ()
appTop = writeBuilder . renderHtmlBuilder . html

appStream :: Chan ServerEvent -> Snap ()
appStream = eventSourceApp

sourceStdIn :: MonadIO m => Enumerator Text m a
sourceStdIn = E.enumHandle stdin

colorConv :: MonadIO m => ColorScheme -> Enumeratee Text Text m a
colorConv cols = E.mapAccum f defaultConsoleState { colorScheme = cols }
  where
    f state text = convHtml state $ parseConsoleString text

textsToEventSource :: Monad m => Enumeratee Text ServerEvent m a
textsToEventSource = E.map f
  where
    f text = ServerEvent { eventName = Nothing
                         , eventId   = Nothing
                         , eventData = [B.fromText text] }

sinkChan :: MonadIO m => Chan a -> Iteratee a m ()
sinkChan chan = E.foldM go ()
  where
    go () a = liftIO $ writeChan chan a
