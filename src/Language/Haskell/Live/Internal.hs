module Language.Haskell.Live.Internal
  ( Session
  , parseResult
  , start
  , stop
  , eval
  , (|>)
  ) where

import Control.Category ((>>>))
import Data.List (isInfixOf)
import Data.String (String)
import qualified Language.Haskell.Ghcid as Ghcid
import Protolude

newtype Session =
  Session Ghcid.Ghci

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

startGhci :: IO Ghcid.Ghci
startGhci = Ghcid.startGhci "ghci" Nothing (\_ _ -> pure ()) |> fmap fst

{-| Start a new session with which code can be evaluated.
Behind the scenes this starts a GHCI process.
-}
start :: IO Session
start =
  Ghcid.startGhci "ghci" Nothing (\_ _ -> pure ()) |> fmap fst |> fmap Session

{-| Eval some Haskell code with the GHCI process.
-}
eval :: Session -> String -> IO EvalResult
eval (Session ghci) source = Ghcid.exec ghci source |> fmap parseResult

{-| Finish the session, cleaning up the GHCI process.
-}
stop :: Session -> IO ()
stop (Session ghci) = Ghcid.stopGhci ghci

type EvalResult = Either String String

{-| Parse a raw result from GHCI to determine whether the previous
evaluation was a success or a failure.
-}
parseResult :: [String] -> EvalResult
parseResult [""] = Right ""
parseResult lines =
  let text = (intersperse "\n" >>> mconcat) lines
      first = lines |> head |> fromMaybe "[]"
  in if | first == "" -> Left text
        | isInfixOf "*** Exception" first -> Left text
        | otherwise -> Right text
