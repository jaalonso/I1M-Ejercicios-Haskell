
{-# OPTIONS_GHC -fno-warn-unused-matches
                -fno-warn-unused-imports
                -fno-warn-orphans
#-}

module El_TAD_de_las_pilas_Spec (main, spec) where

import El_TAD_de_las_pilas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "filtraPila" $
    it "e1" $
      show (filtraPila even ejP1)
      `shouldBe` "2|4|6|8|10|12|14|16|18|20|-"

  describe "mapPila" $
    it "e1" $
      show (mapPila (+7) ejP1)
      `shouldBe` "8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-"

  describe "pertenecePila" $ do
    it "e1" $
      pertenecePila 7 ejP1  `shouldBe` True
    it "e2" $
      pertenecePila 70 ejP1 `shouldBe` False

  describe "contenidaPila" $ do
    it "e1" $
      contenidaPila ejP2 ejP1  `shouldBe` True
    it "e2" $
      contenidaPila ejP1 ejP2  `shouldBe` False

  describe "prefijoPila" $ do
    it "e1" $
      prefijoPila ejP3 ejP2 `shouldBe` False
    it "e2" $
      prefijoPila ejP5 ejP1 `shouldBe` True

  describe "subPila" $ do
    it "e1" $
      subPila ejP2 ejP1 `shouldBe` False
    it "e2" $
      subPila ejP3 ejP1 `shouldBe` True

  describe "ordenadaPila" $ do
    it "e1" $
      ordenadaPila ejP1 `shouldBe` True
    it "e2" $
      ordenadaPila ejP4 `shouldBe` False

  describe "lista2Pila" $
    it "e1" $
      show (lista2Pila [1..6]) `shouldBe` "1|2|3|4|5|6|-"

  describe "pila2Lista" $
    it "e1" $
      pila2Lista ejP2 `shouldBe` [2,5,8,11,14,17]

  describe "ordenaInserPila" $
    it "e1" $
      show (ordenaInserPila ejP4)
      `shouldBe` "-1|0|3|3|3|4|4|7|8|10|-"

  describe "nubPila" $
    it "e1" $
      show (nubPila ejP4)
      `shouldBe` "-1|7|8|10|0|3|4|-"

  describe "maxPila" $
    it "e1" $
      maxPila ejP4 `shouldBe` 10
