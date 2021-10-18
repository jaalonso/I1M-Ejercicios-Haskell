module Definiciones_por_comprension_Spec (main, spec) where

import Definiciones_por_comprension
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "suma" $ do
    it "e1" $
      suma 3 `shouldBe`  6
    it "e2" $
      suma2 3 `shouldBe`  6
    it "e3" $
      property prop_suma

  describe "sumaDeCuadrados" $ do
    it "e1" $
      sumaDeCuadrados 3    `shouldBe`  14
    it "e2" $
      sumaDeCuadrados 100  `shouldBe`  338350
    it "e3" $
      sumaDeCuadrados2 3    `shouldBe`  14
    it "e4" $
      sumaDeCuadrados2 100  `shouldBe`  338350
    it "e5" $
      property prop_sumaDeCuadrados

  describe "euler6" $ do
    it "e1" $
      euler6 10 `shouldBe` 2640
    it "e2" $
      euler6b 10 `shouldBe` 2640

  describe "replica" $ do
    it "e1" $
      replica 4 (7::Int)  `shouldBe`  [7,7,7,7]
    it "e2" $
      replica 3 'a'  `shouldBe`  "aaa"

  describe "linea" $ do
    it "e1" $
      linea 4  `shouldBe`  [7,8,9,10]
    it "e2" $
      linea 5  `shouldBe`  [11,12,13,14,15]

  describe "linea2" $ do
    it "e1" $
      linea2 4  `shouldBe`  [7,8,9,10]
    it "e2" $
      linea2 5  `shouldBe`  [11,12,13,14,15]

  describe "linea3" $ do
    it "e1" $
      linea3 4  `shouldBe`  [7,8,9,10]
    it "e2" $
      linea3 5  `shouldBe`  [11,12,13,14,15]
    it "e3" $
      property prop_linea

  describe "triangulo" $ do
    it "e1" $
      triangulo 3  `shouldBe`  [[1],[2,3],[4,5,6]]
    it "e2" $
      triangulo 4  `shouldBe`  [[1],[2,3],[4,5,6],[7,8,9,10]]

  describe "perfectos" $ do
    it "e1" $
      perfectos 100  `shouldBe`  [6,28]
    it "e2" $
      perfectos 500  `shouldBe`  [6,28,496]

  describe "numeroAbundante" $ do
    it "e1" $
      numeroAbundante 5  `shouldBe` False
    it "e2" $
      numeroAbundante 12 `shouldBe` True
    it "e3" $
      numeroAbundante 28 `shouldBe` False
    it "e4" $
      numeroAbundante 30 `shouldBe` True

  describe "numerosAbundantesMenores" $
    it "e1" $
      numerosAbundantesMenores 50  `shouldBe`  [12,18,20,24,30,36,40,42,48]

  describe "todosPares" $ do
    it "e1" $
      todosPares 10    `shouldBe`  True
    it "e2" $
      todosPares 100   `shouldBe`  True
    it "e3" $
      todosPares 1000  `shouldBe`  False

  describe "primerAbundanteImpar" $
    it "e1" $
      primerAbundanteImpar `shouldBe` 945

  describe "euler1" $ do
    it "e1" $
      euler1 10  `shouldBe`  23
    it "e2" $
      euler1 20  `shouldBe`  78

  describe "circulo" $ do
    it "e1" $
      circulo 3  `shouldBe`  9
    it "e2" $
      circulo 4  `shouldBe`  15
    it "e3" $
      circulo 5  `shouldBe`  22

  describe "circulo2" $ do
    it "e1" $
      circulo2 3  `shouldBe`  9
    it "e2" $
      circulo2 4  `shouldBe`  15
    it "e3" $
      circulo2 5  `shouldBe`  22

  describe "aproxE" $ do
    it "e1" $
      aproxE 1 `shouldBe` [2.0]
    it "e2" $
      aproxE 4 `shouldBe` [2.0,2.25,2.37037037037037,2.44140625]

  describe "errorAproxE" $ do
    it "e1" $
      errorAproxE 0.1    `shouldBe`  13.0
    it "e2" $
      errorAproxE 0.01   `shouldBe`  135.0
    it "e3" $
      errorAproxE 0.001  `shouldBe`  1359.0

  describe "aproxLimSeno" $ do
    it "e1" $
      aproxLimSeno 1 `shouldBe` [0.8414709848078965]
    it "e2" $
      aproxLimSeno 2 `shouldBe` [0.8414709848078965,0.958851077208406]

  describe "errorLimSeno" $ do
    it "e1" $
      errorLimSeno 0.1     `shouldBe`   2.0
    it "e2" $
      errorLimSeno 0.01    `shouldBe`   5.0
    it "e3" $
      errorLimSeno 0.001   `shouldBe`  13.0
    it "e4" $
      errorLimSeno 0.0001  `shouldBe`  41.0

  describe "calculaPi" $ do
    it "e1" $
      calculaPi 3    `shouldBe`  2.8952380952380956
    it "e2" $
      calculaPi 300  `shouldBe`  3.1449149035588526

  describe "errorPi" $ do
    it "e1" $
      errorPi 0.1    `shouldBe`    9.0
    it "e2" $
      errorPi 0.01   `shouldBe`   99.0
    it "e3" $
      errorPi 0.001  `shouldBe`  999.0

  describe "pitagoricas" $
    it "e1" $
      pitagoricas 10  `shouldBe`  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

  describe "numeroDePares" $ do
    it "e1" $
      numeroDePares (3,5,7)  `shouldBe`  0
    it "e2" $
      numeroDePares (3,6,7)  `shouldBe`  1
    it "e3" $
      numeroDePares (3,6,4)  `shouldBe`  2
    it "e4" $
      numeroDePares (4,6,4)  `shouldBe`  3

  describe "conjetura" $
    it "e1" $
      conjetura 10  `shouldBe`  True

  describe "ternasPitagoricas" $ do
    it "e1" $
      ternasPitagoricas 12  `shouldBe`  [(3,4,5)]
    it "e2" $
      ternasPitagoricas 60  `shouldBe`  [(10,24,26),(15,20,25)]

  describe "euler9" $
    it "e1" $
      euler9  `shouldBe`  31875000

  describe "productoEscalar" $
    it "e1" $
      productoEscalar [1,2,3] [4,5,6]  `shouldBe`  32

  describe "sumaConsecutivos" $ do
    it "e1" $
      sumaConsecutivos [3,1,5,2]  `shouldBe`  [4,6,7]
    it "e2" $
      sumaConsecutivos [3]        `shouldBe`  []

  describe "densa" $ do
    it "e1" $
      densa [6,0,-5,4,-7]  `shouldBe`  [(4,6),(2,-5),(1,4),(0,-7)]
    it "e2" $
      densa [6,0,0,3,0,4]  `shouldBe`  [(5,6),(2,3),(0,4)]


  describe "nombres" $
    it "e1" $
      nombres personas `shouldBe`
       ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
        "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]

  describe "musicos" $
    it "e1" $
      musicos personas  `shouldBe`  ["Beethoven","Mozart","Bach"]

  describe "seleccion" $ do
    it "e1" $
      seleccion personas "Pintura"
      `shouldBe` ["Velazquez","Picasso","Goya","Botticelli"]
    it "e2" $
      seleccion personas "Musica"
      `shouldBe` ["Beethoven","Mozart","Bach"]

  describe "musicos'" $
    it "e1" $
      musicos' personas  `shouldBe`  ["Beethoven","Mozart","Bach"]

  describe "vivas" $
    it "e1" $
      vivas personas 1600 `shouldBe`
      ["Cervantes","Velazquez","Quevedo","Borromini"]
