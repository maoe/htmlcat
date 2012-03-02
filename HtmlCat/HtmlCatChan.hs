module HtmlCat.HtmlCatChan (HtmlCatChan, newHtmlCatChan, notifyClient, writeHtmlCatChan, htmlCatChan) where
import Control.Concurrent (Chan, newChan, writeChan, MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (void)

data HtmlCatChan a = HtmlCatChan {
  _firstClient :: MVar (),
  htmlCatChan :: Chan a
}

newHtmlCatChan :: IO (HtmlCatChan a)
newHtmlCatChan = do
  f <- newEmptyMVar
  c <- newChan
  return $ HtmlCatChan f c

notifyClient :: HtmlCatChan a -> IO ()
notifyClient (HtmlCatChan r _) = void $ tryPutMVar r ()

writeHtmlCatChan :: HtmlCatChan a -> a -> IO ()
writeHtmlCatChan (HtmlCatChan r c) a = takeMVar r >> tryPutMVar r () >> writeChan c a
