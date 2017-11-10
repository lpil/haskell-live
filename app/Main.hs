module Main where

import qualified Control.Exception as Exception
import Data.List (isInfixOf)
import Data.String (String)
import qualified Language.Haskell.Live as Live
import Network
import Protolude
import qualified System.IO as SysIO

main :: IO ()
main = do
  bracket Live.start Live.stop socketServer

socketServer :: Live.Session -> IO ()
socketServer session =
  withSocketsDo $ do
    let port = 3001
    sock <- Network.listenOn $ PortNumber port
    putStrLn (">>> Listening on localhost:" <> (show port) :: Text)
    acceptorLoop session sock

acceptorLoop :: Live.Session -> Socket -> IO ()
acceptorLoop session socket = do
  (handle, host, port) <- accept socket
  let clientLabel = ((host <> ":" <> show port) :: String)
  putStrLn ("CONNECT " <> clientLabel)
  connectionLoop session handle
  putStrLn ("DISCONNECT " <> clientLabel <> "\n")
  acceptorLoop session socket

connectionLoop :: Live.Session -> Handle -> IO ()
connectionLoop session handle = do
  readInput handle >>= \case
    Just input -> do
      putStrLn ("IN  " <> input)
      result <- Live.eval session (wrapInput input)
      putStrLn ((("OUT " <> show result)) :: Text)
      connectionLoop session handle
    Nothing -> do
      SysIO.hClose handle
      pure ()

readInput :: Handle -> IO (Maybe String)
readInput handle =
  Exception.catch (fmap Just (SysIO.hGetContents handle)) handleIOException

handleIOException :: IOException -> IO (Maybe String)
handleIOException _ = pure Nothing

wrapInput :: String -> String
wrapInput input =
  traceShowId $ ":{\n" <> (replace "\n\n" ":}\n:{\n" input) <> "\n:}"

replace :: String -> String -> String -> String
replace target replacement [] = []
replace target replacement subject@(x:xs)
  | isInfixOf target subject =
    traceShowId $
    replacement ++ (replace target replacement (drop (length target) subject))
  | otherwise = traceShowId $ x : (replace target replacement xs)
