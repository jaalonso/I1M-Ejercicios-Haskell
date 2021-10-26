module Algoritmos_de_ordenacion_y_complejidad_Spec (main, spec) where

import Algoritmos_de_ordenacion_y_complejidad
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ordenaPorSeleccion" $ do
    it "e1" $
      ordenaPorSeleccion [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]
    it "e2" $
      ordenaPorSeleccion2 [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]

  describe "ordenaRapida" $ do
    it "e1" $
      ordenaRapida [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]
    it "e2" $
      ordenaRapida2 [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]

  describe "ordenaPorInsercion" $ do
    it "e1" $
      ordenaPorInsercion [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]
    it "e2" $
      ordenaPorInsercion2 [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]

  describe "ordenaPorMezcla" $ do
    it "e1" $
      ordenaPorMezcla [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]
    it "e2" $
      ordenaPorMezcla2 [3,1,4,1,5,9,2]  `shouldBe`  [1,1,2,3,4,5,9]
