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
import qualified Data.Enumerator.List as E (map, foldM)
import qualified Data.Enumerator.Text as E (enumHandle)

import HtmlCat.Html (html)
import Snap.EventSource (ServerEvent(..), eventSourceApp)

feedStdIn :: Chan ServerEvent -> IO ()
feedStdIn chan = void . forkIO $ E.run_ $
  sourceStdIn $= textsToEventSource $$ sinkChan chan

runHtmlCat :: Chan ServerEvent -> String -> Int -> IO ()
runHtmlCat chan host port =
  simpleHttpServe (setPort port $ setBind (B8.pack host)
                                $ defaultConfig :: Config Snap ())
                  (app chan)

app :: Chan ServerEvent -> Snap ()
app chan = route [ ("",       appTop)
                 , ("stream", appStream chan)
                 ]

appTop :: Snap ()
appTop = writeBuilder $ renderHtmlBuilder html

appStream :: Chan ServerEvent -> Snap ()
appStream = eventSourceApp

sourceStdIn :: MonadIO m => Enumerator Text m a
sourceStdIn = E.enumHandle stdin

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
