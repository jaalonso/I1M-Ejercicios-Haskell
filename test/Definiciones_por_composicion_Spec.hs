module Definiciones_por_composicion_Spec (main, spec) where

import Definiciones_por_composicion
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "media3" $ do
    it "e1" $
      media3 1 3 8     `shouldBe`  4.0
    it "e2" $
      media3 (-1) 0 7  `shouldBe`  2.0
    it "e3" $
      media3 (-3) 0 3  `shouldBe`  0.0

  describe "sumaMonedas" $ do
    it "e1" $
      sumaMonedas 0 0 0 0 1  `shouldBe`  20
    it "e2" $
      sumaMonedas 0 0 8 0 3  `shouldBe` 100
    it "e3" $
      sumaMonedas 1 1 1 1 1  `shouldBe`  38

  describe "volumenEsfera" $ do
    it "e1" $
      volumenEsfera 10  `shouldBe`  4188.790204786391

  describe "areaDeCoronaCircular" $ do
    it "e1" $
      areaDeCoronaCircular 1 2 `shouldBe` 9.42477796076938
    it "e2" $
      areaDeCoronaCircular 2 5 `shouldBe` 65.97344572538566
    it "e3" $
      areaDeCoronaCircular 3 5 `shouldBe` 50.26548245743669

  describe "ultimaCifra" $ do
    it "e1" $
      ultimaCifra 325  `shouldBe`  5

  describe "maxTres" $ do
    it "e1" $
      maxTres 6 2 4  `shouldBe`  6
    it "e2" $
      maxTres 6 7 4  `shouldBe`  7
    it "e3" $
      maxTres 6 7 9  `shouldBe`  9

  describe "rota1" $ do
    it "e1" $
      rota1 [3,2,5,7]  `shouldBe`  [2,5,7,3]

  describe "rota" $ do
    it "e1" $
      rota 1 [3,2,5,7]  `shouldBe`  [2,5,7,3]
    it "e2" $
      rota 2 [3,2,5,7]  `shouldBe`  [5,7,3,2]
    it "e3" $
      rota 3 [3,2,5,7]  `shouldBe`  [7,3,2,5]

  describe "rango" $ do
    it "e1" $
      rango [3,2,7,5]  `shouldBe`  [2,7]

  describe "palindromo" $ do
    it "e1" $
      palindromo [3,2,5,2,3]    `shouldBe`  True
    it "e2" $
      palindromo [3,2,5,6,2,3]  `shouldBe`  False

  describe "interior" $ do
    it "e1" $
      interior [2,5,3,7,3]  `shouldBe`  [5,3,7]
    it "e2" $
      interior [2..7]       `shouldBe`  [3,4,5,6]

  describe "finales" $ do
    it "e1" $
      finales 3 [2,5,4,7,9,6]  `shouldBe`  [7,9,6]

  describe "segmento" $ do
    it "e1" $
      segmento 3 4 [3,4,1,2,7,9,0]  `shouldBe`  [1,2]
    it "e2" $
      segmento 3 5 [3,4,1,2,7,9,0]  `shouldBe`  [1,2,7]
    it "e3" $
      segmento 5 3 [3,4,1,2,7,9,0]  `shouldBe`  []

  describe "extremos" $ do
    it "e1" $
      extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  `shouldBe`  [2,6,7,9,2,3]

  describe "mediano" $ do
    it "e1" $
      mediano 3 2 5  `shouldBe`  3
    it "e2" $
      mediano 2 4 5  `shouldBe`  4
    it "e3" $
      mediano 2 6 5  `shouldBe`  5
    it "e4" $
      mediano 2 6 6  `shouldBe`  6

  describe "tresIguales" $ do
    it "e1" $
      tresIguales 4 4 4  `shouldBe`  True
    it "e2" $
      tresIguales 4 3 4  `shouldBe`  False

  describe "tresDiferentes" $ do
    it "e1" $
      tresDiferentes 3 5 2  `shouldBe`  True
    it "e2" $
      tresDiferentes 3 5 3  `shouldBe`  False

  describe "cuatroIguales" $ do
    it "e1" $
      cuatroIguales 5 5 5 5   `shouldBe`  True
    it "e2" $
      cuatroIguales 5 5 4 5   `shouldBe`  False
