module Language.Haskell.Live
  ( run
  ) where

import Control.Category ((>>>))
import Data.String (String)
import qualified Language.Haskell.Ghcid as Ghcid
import Protolude

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

run :: IO ()
run = bracket startGhci Ghcid.stopGhci runLive

startGhci :: IO Ghcid.Ghci
startGhci = Ghcid.startGhci "ghci" Nothing (\_ _ -> pure ()) |> fmap fst

runLive :: Ghcid.Ghci -> IO ()
runLive ghci =
  [ "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "pack []"
  , "import Data.Text"
  , "pack []"
  ] |>
  mapM_ (execPrint ghci)

execPrint :: Ghcid.Ghci -> String -> IO ()
execPrint ghci source = do
  result <- Ghcid.exec ghci source
  (intersperse "\n" >>> mconcat >>> putStrLn) result
