{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module El_TAD_de_los_monticulos_Spec (main, spec) where

import El_TAD_de_los_monticulos
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =   do
  describe "numeroDeNodos" $
    it "e1" $
      numeroDeNodos m1  `shouldBe`  4

  describe "filtra" $ do
    it "e1" $
      show (filtra even m1)
      `shouldBe` "M 4 1 (M 6 1 (M 8 1 Vacio Vacio) Vacio) Vacio"
    it "e2" $
      show (filtra odd m1)
      `shouldBe` "M 1 1 Vacio Vacio"

  describe "menores" $ do
    it "e1" $
      menores 3 m1 `shouldBe` [1,4,6]
    it "e2" $
      menores 10 m1 `shouldBe` [1,4,6,8]

  describe "restantes" $ do
    it "e1" $
      show (restantes 3 m1)
      `shouldBe` "M 8 1 Vacio Vacio"
    it "e2" $
      show (restantes 2 m1)
      `shouldBe` "M 6 1 (M 8 1 Vacio Vacio) Vacio"
    it "e3" $
      show (restantes 7 m1)
      `shouldBe` "Vacio"

  describe "lista2Monticulo" $
    it "e1" $
      show (lista2Monticulo [2,5,3,7])
      `shouldBe` "M 2 1 (M 3 2 (M 7 1 Vacio Vacio) (M 5 1 Vacio Vacio)) Vacio"

  describe "monticulo2Lista" $
    it "e1" $
      monticulo2Lista m1 `shouldBe` [1,4,6,8]

  describe "ordenada" $ do
    it "e1" $
      ordenada [3,5,9]  `shouldBe`  True
    it "e2" $
      ordenada [3,5,4]  `shouldBe`  False
    it "e3" $
      ordenada [7,5,4]  `shouldBe`  False

  describe "prop_monticulo2Lista_ordenada" $
    it "e1" $
      property prop_monticulo2Lista_ordenada

  describe "ordena" $ do
    it "e1" $
      ordena [7,5,3,6,5]  `shouldBe`  [3,5,5,6,7]
    it "e2" $
      property prop_ordena_ordenada

  describe "borra" $ do
    it "e1" $
      borra 1 [1,2,1]  `shouldBe`  [2,1]
    it "e2" $
      borra 3 [1,2,1]  `shouldBe`  [1,2,1]

  describe "esPermutacion" $ do
    it "e1" $
      esPermutacion [1,2,1] [2,1,1]  `shouldBe`  True
    it "e2" $
      esPermutacion [1,2,1] [1,2,2]  `shouldBe`  False

  describe "prop_ordena_permutacion" $
    it "e1" $
      property prop_ordena_permutacion
