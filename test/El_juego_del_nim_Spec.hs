module El_juego_del_nim_Spec (main, spec) where

import El_juego_del_nim
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "finalizado" $ do
    it "e1" $
      finalizado [0,0,0,0,0]  `shouldBe`  True
    it "e2" $
      finalizado [1,3,0,0,1]  `shouldBe`  False

  describe "valida" $ do
    it "e1" $
      valida [4,3,2,1,0] 2 3  `shouldBe`  True
    it "e2" $
      valida [4,3,2,1,0] 2 4  `shouldBe`  False
    it "e3" $
      valida [4,3,2,1,0] 2 2  `shouldBe`  True
    it "e4" $
      valida [4,3,2,1,0] 2 0  `shouldBe`  False

  describe "jugada" $
    it "e1" $
      jugada [4,3,2,1,0] 2 1  `shouldBe`  [4,2,2,1,0]

  describe "estrellas" $
    it "e1" $
      estrellas 3  `shouldBe`  "* * * "
