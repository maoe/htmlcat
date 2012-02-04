{-# LANGUAGE ScopedTypeVariables, RecordWildCards, CPP, DeriveDataTypeable #-}
module Main where
import Control.Concurrent (forkIO, newChan)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Foldable (forM_)
import Data.Maybe (maybeToList)
import Network (PortID(..), listenOn, sClose)
import System.Process (rawSystem)

import System.Console.CmdArgs

#ifdef SNAP
import HtmlCat.Snap
#else
import HtmlCat.Wai
#endif

main :: IO ()
main = do
  HtmlCat {..} <- cmdArgs htmlCat
  port <- newPort _port
  let url = "http://" ++ _host ++ ":" ++ show port
  putStrLn url
  whenJust _exec $ \exec ->
    forkIO $ void $ rawSystem exec [url]
  chan <- newChan
  feedStdIn chan
  runHtmlCat chan _host port
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
