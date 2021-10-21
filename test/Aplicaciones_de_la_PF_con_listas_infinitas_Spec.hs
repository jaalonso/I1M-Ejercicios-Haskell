module Aplicaciones_de_la_PF_con_listas_infinitas_Spec (main, spec) where

import Aplicaciones_de_la_PF_con_listas_infinitas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "enteros" $
    it "e1" $
      take 10 enteros  `shouldBe`  [0,-1,1,-2,2,-3,3,-4,4,-5]

  describe "enteros2" $
    it "e1" $
      take 10 enteros2  `shouldBe`  [0,-1,1,-2,2,-3,3,-4,4,-5]

  describe "posicion" $
    it "e1" $
      posicion 2  `shouldBe`  4

  describe "posicion2" $
    it "e1" $
      posicion2 2  `shouldBe`  4

  describe "posicion3" $
    it "e1" $
      posicion3 2  `shouldBe`  4

  describe "posicion4" $
    it "e1" $
      posicion4 2  `shouldBe`  4

  describe "eslabones" $
    it "e1" $
      take 10 (eslabones 2 7 25)  `shouldBe`  [2,9,16,23,5,12,19,1,8,15]

  describe "eslabones2" $
    it "e1" $
      take 10 (eslabones2 2 7 25)  `shouldBe`  [2,9,16,23,5,12,19,1,8,15]

  describe "numeroVueltas" $
    it "e1" $
      numeroVueltas 2 7 25  `shouldBe`  14

  describe "golomb" $ do
    it "e1" $
      golomb 5  `shouldBe`  3
    it "e2" $
      golomb 9  `shouldBe`  5

  describe "sucGolomb" $
    it "e1" $
      take 15 sucGolomb  `shouldBe`  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]

  describe "subsucGolomb" $
    it "e1" $
      take 10 (subSucGolomb 4)  `shouldBe`  [4,4,4,5,5,5,6,6,6,6]
