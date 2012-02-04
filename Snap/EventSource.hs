{-# LANGUAGE OverloadedStrings #-}
module Snap.EventSource
    ( ServerEvent(..)
    , eventSourceApp
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Chan (Chan, readChan, dupChan)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid (mappend, mconcat)

import Blaze.ByteString.Builder (Builder, flush)
import Blaze.ByteString.Builder.Char8 (fromString, fromChar, fromShow)
import Data.Enumerator.List (generateM)
import Snap.Core (MonadSnap, modifyResponse, setContentType, setResponseBody)

eventSourceApp :: MonadSnap m => Chan ServerEvent -> m ()
eventSourceApp origChan = do
  chan <- liftIO $ dupChan origChan
  modifyResponse
    $ setContentType "text/event-stream"
    . setResponseBody (generateM (eventToBuilder <$> readChan chan))

------------------------------------------------------------
-- Internal implementation
-- Quoted from https://github.com/cdsmith/gloss-web/blob/master/src/EventStream.hs

data ServerEvent
  = ServerEvent { eventName :: Maybe Builder
                , eventId   :: Maybe Builder
                , eventData :: [Builder] }
  | CommentEvent { eventComment :: Builder }
  | RetryEvent { eventRetry :: Int }
  | CloseEvent

nl :: Builder
nl = fromChar '\n'

nameField, idField, dataField, retryField, commentField :: Builder
nameField = fromString "event:"
idField = fromString "id:"
dataField = fromString "data:"
retryField = fromString "retry:"
commentField = fromChar ':'

field :: Builder -> Builder -> Builder
field l b = l `mappend` b `mappend` nl

flushAfter :: Builder -> Builder
flushAfter b = b `mappend` flush

eventToBuilder :: ServerEvent -> Maybe Builder
eventToBuilder (CommentEvent txt)  = Just $ flushAfter $ field commentField txt
eventToBuilder (RetryEvent n)      = Just $ flushAfter $ field retryField (fromShow n)
eventToBuilder CloseEvent          = Nothing
eventToBuilder (ServerEvent n i d) = Just $ flushAfter $
  (name n $ evid i $ mconcat (map (field dataField) d)) `mappend` nl
  where
    name Nothing  = id
    name (Just n') = mappend (field nameField n')
    evid Nothing  = id
    evid (Just i') = mappend (field idField   i')
