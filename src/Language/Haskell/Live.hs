module Language.Haskell.Live
  ( Live.Session
  , run
  , Live.eval
  , Live.start
  , Live.stop
  ) where

import Control.Category ((>>>))
import Data.String (String)
import Language.Haskell.Live.Internal as Live
import Language.Haskell.Live.Internal ((|>))
import Protolude

run :: IO ()
run = bracket Live.start Live.stop evalStatements

evalStatements :: Live.Session -> IO ()
evalStatements session =
  [ "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "1 + 1"
  , "pack \"hello\""
  , "import Data.Text"
  , "undefined"
  , "pack \"hello\""
  ] |>
  mapM_ (evalLine session)

evalLine :: Session -> String -> IO ()
evalLine session source =
  Live.eval session source |> fmap show >>= (putStrLn :: String -> IO ())
