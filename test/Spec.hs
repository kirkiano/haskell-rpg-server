
module Main where

main :: IO ()
main = return ()

{- ( main ) where

import Control.Monad                        ( void )
import Data.Either.Combinators              ( isRight )
import qualified Data.Map.Strict            as M
import qualified RPGServer.World            as W
import RPGServer.DB.Error                   ( DBError(..) )
import RPGServer.DB.Cache                   ( Cache(..) )
import RPGServer.DB.Memory                  ( World, emptyWorld )
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "address" $ do
    it "adds a new address" $ do
      let aid = 42
          a   = W.Address aid "Banks house" 23 "Cherry Tree Lane" "London" "UK"
          w0  = emptyWorld
      address w0 aid `shouldBe` Left (NoSuchAddress aid)
      let w1E = upsertAddress w0 a
      w1E `shouldSatisfy` isRight
      let (Right w1) = w1E
      address w1 aid `shouldBe` Right a
      hspec $ do
        describe "place" $ do
          it "adds a new place" $ do
            let pid   = 1
                pname = "vestibule"
                pdesc = "spartan"
                p   = W.PlaceWithAID     pid pname pdesc $ Just aid
                pA  = W.PlaceWithAddress pid pname pdesc $ Just a
                w2E = upsertPlace w1 p
            w2E `shouldSatisfy` isRight
            let (Right w2) = w2E
            place w2 pid `shouldBe` Right pA
-}
