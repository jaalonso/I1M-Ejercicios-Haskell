module Estadistica_descriptiva_Spec (main, spec) where

import Estadistica_descriptiva
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "media" $
    it "e1" $
      media [4,8,4,5,9]  `shouldBe`  6.0

  describe "mediana" $ do
    it "e1" $
      mediana [2,3,6,8,9]    `shouldBe`  6.0
    it "e2" $
      mediana [2,3,4,6,8,9]  `shouldBe`  5.0
    it "e3" $
      mediana [9,6,8,4,3,2]  `shouldBe`  5.0
    it "e4" $
      property prop_mediana

  describe "frecuencias" $
    it "e1" $
      frecuencias "sosos" `shouldBe`  [('o',2),('s',3)]

  describe "modas" $
    it "e1" $
      modas [7,3,7,5,3,1,6,9,6]  `shouldBe`  [3,6,7]

  describe "mediaGeometrica" $ do
    it "e1" $
      mediaGeometrica [2,18]   `shouldBe`  6.0
    it "e2" $
      mediaGeometrica [3,1,9]  `shouldBe`  3.0
    it "e3" $
      property prop_mediaGeometrica

  describe "rango" $
    it "e1" $
      rango [4,2,4,7,3]  `shouldBe`  5

  describe "desviacionMedia" $ do
    it "e1" $
      desviacionMedia [4,8,4,5,9]       `shouldBe`  2.0
    it "e2" $
      desviacionMedia (replicate 10 3)  `shouldBe`  0.0

  describe "varianza" $ do
    it "e1" $
      varianza [4,8,4,5,9]       `shouldBe`  4.4
    it "e2" $
      varianza (replicate 10 3)  `shouldBe`  0.0

  describe "desviacionTipica" $ do
    it "e1" $
      desviacionTipica [4,8,4,5,9]       `shouldBe`  2.0976176963403033
    it "e2" $
      desviacionTipica (replicate 10 3)  `shouldBe`  0.0

  describe "regresionLineal" $ do
    it "e1" $
      regresionLineal ejX ejY `shouldBe`
      (5.195045748716805,0.9218924347243919)
