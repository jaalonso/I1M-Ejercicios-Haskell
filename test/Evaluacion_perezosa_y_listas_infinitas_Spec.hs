module Evaluacion_perezosa_y_listas_infinitas_Spec (main, spec) where

import Evaluacion_perezosa_y_listas_infinitas
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "repite" $ do
    it "e1" $
      take 3 (repite1 (5::Int))  `shouldBe`  [5,5,5]
    it "e2" $
      take 3 (repite2 (5::Int))  `shouldBe`  [5,5,5]
    it "e3" $
      take 3 (repite (5::Int))  `shouldBe`  [5,5,5]

  describe "repiteC" $
    it "e1" $
      take 3 (repiteC (5::Int))  `shouldBe`  [5,5,5]

  describe "repiteFinitaR" $
    it "e1" $
      repiteFinitaR 3 (5::Int)  `shouldBe`  [5,5,5]

  describe "repiteFinitaC" $
    it "e1" $
      repiteFinitaC 3 (5::Int)  `shouldBe`  [5,5,5]

  describe "repiteFinita" $
    it "e1" $
      repiteFinita 3 (5::Int)  `shouldBe`  [5,5,5]

  describe "repiteFinita2" $
    it "e1" $
      repiteFinita2 3 (5::Int)  `shouldBe`  [5,5,5]

  describe "prop_repiteFinita" $
    modifyMaxSize (const 20) $ do
      it "p1" $
        property prop_repiteFinitaEquiv
      it "p2" $
        property prop_repiteFinitaLongitud
      it "p3" $
        property prop_repiteFinitaLongitud2
      it "p4" $
        property prop_repiteFinitaIguales

  describe "ecoC" $ do
    it "e1" $
      ecoC "abcd"  `shouldBe`  "abbcccdddd"
    it "e1" $
      ecoC2 "abcd"  `shouldBe`  "abbcccdddd"

  describe "ecoR" $
    it "e1" $
      ecoR "abcd"  `shouldBe`  "abbcccdddd"

  describe "itera" $ do
    it "e1" $
      take 7 (itera (+1) 3) `shouldBe` [3,4,5,6,7,8,9]
    it "e2" $
      take 7 (itera (*2) 1) `shouldBe` [1,2,4,8,16,32,64]
    it "e3" $
      take 7 (itera (`div` 10) 1972) `shouldBe` [1972,197,19,1,0,0,0]

  describe "agrupaR" $ do
    it "e1" $
      agrupaR 2 [3,1,5,8,2,7] `shouldBe` [[3,1],[5,8],[2,7]]
    it "e2" $
      agrupaR 2 [3,1,5,8,2,7,9] `shouldBe` [[3,1],[5,8],[2,7],[9]]
    it "e3" $
      agrupaR 5 "todo necio confunde valor y precio"
      `shouldBe` ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]

  describe "agrupa" $ do
    it "e1" $
      agrupa 2 [3,1,5,8,2,7] `shouldBe` [[3,1],[5,8],[2,7]]
    it "e2" $
      agrupa 2 [3,1,5,8,2,7,9] `shouldBe` [[3,1],[5,8],[2,7],[9]]
    it "e3" $
      agrupa 5 "todo necio confunde valor y precio"
      `shouldBe` ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]
    it "p1" $
      property prop_AgrupaLongitud
    it "p2" $
      property prop_AgrupaCombina

  describe "siguiente" $ do
    it "e1" $
      siguiente 13  `shouldBe`  40
    it "e2" $
      siguiente 40  `shouldBe`  20

  describe "collatzR" $
    it "e1" $
      collatzR 13  `shouldBe`  [13,40,20,10,5,16,8,4,2,1]

  describe "collatz" $
    it "e1" $
      collatz 13  `shouldBe`  [13,40,20,10,5,16,8,4,2,1]

  describe "menorCollatzMayor" $
    it "e1" $
      menorCollatzMayor 100  `shouldBe`  27

  describe "menorCollatzSupera" $ do
    it "e1" $
      menorCollatzSupera 100  `shouldBe`  15
    it "e2" $
      menorCollatzSupera2 100  `shouldBe`  15

  describe "potenciasMenores" $
    it "e1" $
      potenciasMenores 2 1000  `shouldBe`  [2,4,8,16,32,64,128,256,512]

  describe "primos" $
    it "e1" $
      take 10 primos  `shouldBe`  [2,3,5,7,11,13,17,19,23,29]

  describe "primo" $ do
    it "e1" $
      primo 7  `shouldBe`  True
    it "e2" $
      primo 9  `shouldBe`  False

  describe "sumaDeDosPrimos" $ do
    it "e1" $
      sumaDeDosPrimos 30  `shouldBe`  [(7,23),(11,19),(13,17)]
    it "e2" $
      sumaDeDosPrimos 10  `shouldBe`  [(3,7),(5,5)]

  describe "factoriales1" $
    it "e1" $
      take 10 factoriales1  `shouldBe`  [1,1,2,6,24,120,720,5040,40320,362880]

  describe "factoriales2" $
    it "e1" $
      take 10 factoriales2  `shouldBe`  [1,1,2,6,24,120,720,5040,40320,362880]

  describe "factoriales3" $
    it "e1" $
      take 10 factoriales3  `shouldBe`  [1,1,2,6,24,120,720,5040,40320,362880]

  describe "factoriales4" $
    it "e1" $
      take 10 factoriales4  `shouldBe`  [1,1,2,6,24,120,720,5040,40320,362880]

  describe "factoriales5" $
    it "e1" $
      take 10 factoriales5  `shouldBe`  [1,1,2,6,24,120,720,5040,40320,362880]

  describe "fib" $
    it "e1" $
      fib 8  `shouldBe`  21

  describe "fibs1" $
    it "e1" $
      take 10 fibs1  `shouldBe`  [0,1,1,2,3,5,8,13,21,34]

  describe "fibs2" $
    it "e1" $
      take 10 fibs2  `shouldBe`  [0,1,1,2,3,5,8,13,21,34]

  describe "fibs3" $
    it "e1" $
      take 10 fibs3  `shouldBe`  [0,1,1,2,3,5,8,13,21,34]

  describe "fibs4" $
    it "e1" $
      take 10 fibs4  `shouldBe`  [0,1,1,2,3,5,8,13,21,34]

  describe "pascal1" $
    it "e1" $
      take 6 pascal1
      `shouldBe` [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

  describe "pascal2" $
    it "e1" $
      take 6 pascal2
      `shouldBe` [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
