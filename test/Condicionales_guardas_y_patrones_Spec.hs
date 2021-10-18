-- Condicionales_guardas_y_patrones_Spec.hs
-- Definiciones con condicionales, guardas o patrones.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Condicionales_guardas_y_patrones_Spec (main, spec) where

import Condicionales_guardas_y_patrones
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "divisionSegura" $ do
    it "e1" $
      divisionSegura 7 2  `shouldBe`  3.5
    it "e2" $
      divisionSegura 7 0  `shouldBe`  9999.0

  describe "xor1" $ do
    it "e1" $
      xor1 True  True  `shouldBe` False
    it "e2" $
      xor1 True  False `shouldBe` True
    it "e3" $
      xor1 False True  `shouldBe` True
    it "e4" $
      xor1 False False `shouldBe` False

  describe "xor2" $ do
    it "e1" $
      xor2 True  True  `shouldBe` False
    it "e2" $
      xor2 True  False `shouldBe` True
    it "e3" $
      xor2 False True  `shouldBe` True
    it "e4" $
      xor2 False False `shouldBe` False

  describe "xor3" $ do
    it "e1" $
      xor3 True  True  `shouldBe` False
    it "e2" $
      xor3 True  False `shouldBe` True
    it "e3" $
      xor3 False True  `shouldBe` True
    it "e4" $
      xor3 False False `shouldBe` False

  describe "xor3b" $ do
    it "e1" $
      xor3b True  True  `shouldBe` False
    it "e2" $
      xor3b True  False `shouldBe` True
    it "e3" $
      xor3b False True  `shouldBe` True
    it "e4" $
      xor3b False False `shouldBe` False

  describe "xor4" $ do
    it "e1" $
      xor4 True  True  `shouldBe` False
    it "e2" $
      xor4 True  False `shouldBe` True
    it "e3" $
      xor4 False True  `shouldBe` True
    it "e4" $
      xor4 False False `shouldBe` False

  describe "mayorRectangulo" $ do
    it "e1" $
      mayorRectangulo (4,6) (3,7)  `shouldBe`  (4,6)
    it "e2" $
      mayorRectangulo (4,6) (3,8)  `shouldBe`  (4,6)
    it "e3" $
      mayorRectangulo (4,6) (3,9)  `shouldBe`  (3,9)

  describe "intercambia" $ do
    it "e1" $
      intercambia ((2,5) :: (Int,Int))  `shouldBe`  (5,2)
    it "e2" $
      intercambia ((5,2) :: (Int,Int)) `shouldBe`  (2,5)

  describe "distancia" $
    it "e1" $
      distancia (1,2) (4,6)  `shouldBe`  5.0

  describe "ciclo" $ do
    it "e1" $
      ciclo [2::Int,5,7,9]  `shouldBe` [9,2,5,7]
    it "e2" $
      ciclo ([] :: [Int]) `shouldBe` []
    it "e3" $
      ciclo [2::Int]        `shouldBe` [2]

  describe "numeroMayor" $ do
    it "e1" $
      numeroMayor (2::Int) 5 `shouldBe`  52
    it "e2" $
      numeroMayor (5::Int) 2 `shouldBe`  52

  describe "numeroMayor2" $ do
    it "e1" $
      numeroMayor2 (2::Int) 5 `shouldBe`  52
    it "e2" $
      numeroMayor2 (5::Int) 2 `shouldBe`  52

  describe "numeroDeRaices" $ do
    it "e1" $
      numeroDeRaices 2 0 3    `shouldBe`  0
    it "e2" $
      numeroDeRaices 4 4 1    `shouldBe`  1
    it "e2" $
      numeroDeRaices 5 23 12  `shouldBe`  2

  describe "numeroDeRaices2" $ do
    it "e1" $
      numeroDeRaices2 2 0 3    `shouldBe`  0
    it "e2" $
      numeroDeRaices2 4 4 1    `shouldBe`  1
    it "e2" $
      numeroDeRaices2 5 23 12  `shouldBe`  2

  describe "raices" $ do
    it "e1" $
      raices 1 3 2    `shouldBe`  [-1.0,-2.0]
    it "e2" $
      raices 1 (-2) 1 `shouldBe`  [1.0,1.0]
    it "e3" $
      raices 1 0 1    `shouldBe`  []

  describe "area" $
    it "e1" $
      area 3 4 5  `shouldBe`  6.0

  describe "interseccion" $ do
    it "e1" $
      interseccion [] [3::Int,5]     `shouldBe`  []
    it "e2" $
      interseccion [3::Int,5] []     `shouldBe`  []
    it "e3" $
      interseccion [2::Int,4] [6,9]  `shouldBe`  []
    it "e4" $
      interseccion [2::Int,6] [6,9]  `shouldBe`  [6,6]
    it "e5" $
      interseccion [2::Int,6] [0,9]  `shouldBe`  [2,6]
    it "e6" $
      interseccion [2::Int,6] [0,4]  `shouldBe`  [2,4]
    it "e7" $
      interseccion [4::Int,6] [0,4]  `shouldBe`  [4,4]
    it "e8" $
      interseccion [5::Int,6] [0,4]  `shouldBe`  []

  describe "formaReducida" $ do
    it "e1" $
      formaReducida (4,10)  `shouldBe`  (2,5)
    it "e2" $
      formaReducida (0,5)   `shouldBe`  (0,1)

  describe "sumaRacional" $ do
    it "e1" $
      sumaRacional (2,3) (5,6)  `shouldBe`  (3,2)
    it "e2" $
      sumaRacional (3,5) (-3,5) `shouldBe`  (0,1)

  describe "productoRacional" $
    it "e1" $
      productoRacional (2,3) (5,6)  `shouldBe`  (5,9)

  describe "igualdadRacional" $ do
    it "e1" $
      igualdadRacional (6,9) (10,15)  `shouldBe`  True
    it "e2" $
      igualdadRacional (6,9) (11,15)  `shouldBe`  False
    it "e3" $
      igualdadRacional (0,2) (0,-5)   `shouldBe`  True
