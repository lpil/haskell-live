module Language.Haskell.Live.InternalSpec
  ( main
  , spec
  ) where

import Language.Haskell.Live.Internal as Live
import Protolude
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "parseResult" $ do
    it "parses ok values" $ do
      Live.parseResult ["2"] `shouldBe` Right "2"
      Live.parseResult ["[", "]"] `shouldBe` Right "[\n]"
      Live.parseResult ["Constructor"] `shouldBe` Right "Constructor"
      Live.parseResult [""] `shouldBe` Right ""
      Live.parseResult [] `shouldBe` Right ""
    it "parses errors" $ do
      Live.parseResult
        [ ""
        , "<interactive>:37:1: error:"
        , "    Variable not in scope: pack :: [Char] -> t"
        ] `shouldBe`
        Left
          "\n<interactive>:37:1: error:\n    Variable not in scope: pack :: [Char] -> t"
      Live.parseResult
        [ "*** Exception: Prelude.undefined"
        , "CallStack (from HasCallStack):"
        , "  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err"
        , "  undefined, called at src/Debug.hs:65:13 in protolude-0.1.10-EbWghKT4Ra36YSCOzDFDKT:Debug"
        ] `shouldBe`
        Left
          "*** Exception: Prelude.undefined\nCallStack (from HasCallStack):\n  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err\n  undefined, called at src/Debug.hs:65:13 in protolude-0.1.10-EbWghKT4Ra36YSCOzDFDKT:Debug"
