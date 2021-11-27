module El_triangulo_de_Floyd_Spec (main, spec) where

import El_triangulo_de_Floyd
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "triangulares" $ do
    it "e1" $
      take 10 triangulares     `shouldBe`  [1,3,6,10,15,21,28,36,45,55]
    it "e2" $
      take 10 triangulares2     `shouldBe`  [1,3,6,10,15,21,28,36,45,55]
    it "e3" $
      take 10 triangulares3     `shouldBe`  [1,3,6,10,15,21,28,36,45,55]

  describe "siguienteF" $ do
    it "e1" $
      siguienteF [2,3]    `shouldBe`  [4,5,6]
    it "e2" $
      siguienteF [4,5,6]  `shouldBe`  [7,8,9,10]

  describe "trianguloFloyd" $
    it "e1" $
      take 4 trianguloFloyd `shouldBe`
      [[1],
       [2,3],
       [4,5,6],
       [7,8,9,10]]

  describe "filaTrianguloFloyd" $ do
    it "e1" $
      filaTrianguloFloyd 3  `shouldBe`  [4,5,6]
    it "e2" $
      filaTrianguloFloyd 4  `shouldBe`  [7,8,9,10]

  describe "sumaFilaTrianguloFloyd" $ do
    it "e1" $
      sumaFilaTrianguloFloyd 1  `shouldBe`  1
    it "e2" $
      sumaFilaTrianguloFloyd 2  `shouldBe`  5
    it "e3" $
      sumaFilaTrianguloFloyd 3  `shouldBe`  15
    it "e4" $
      sumaFilaTrianguloFloyd 4  `shouldBe`  34
    it "e5" $
      sumaFilaTrianguloFloyd 5  `shouldBe`  65

  describe " prop_sumaFilaTrianguloFloyd" $
    it "e1" $
      property prop_sumaFilaTrianguloFloyd

  describe "hipotenusaFloyd" $ do
    it "e1" $
      take 5 hipotenusaFloyd  `shouldBe`  [1,3,6,10,15]
    it "e2" $
      prop_hipotenusaFloyd 1000
      `shouldBe` True
