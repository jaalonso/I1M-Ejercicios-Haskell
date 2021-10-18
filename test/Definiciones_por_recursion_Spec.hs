module Definiciones_por_recursion_Spec (main, spec) where

import Definiciones_por_recursion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "potencia" $
    it "e1" $
      potencia 2 3  `shouldBe`  8

  describe "prop_potencia" $
    it "p1" $
      property prop_potencia

  describe "mcd" $
    it "e1" $
      mcd 30 45  `shouldBe`  15

  describe "prop_mcd" $
    it "p1" $
      property  prop_mcd

  describe "prop_mcd_div'" $
    it "p1" $
      property  prop_mcd_div'

  describe "pertenece" $ do
    it "e1" $
      pertenece (3::Int) [2,3,5]  `shouldBe`  True
    it "e3" $
      pertenece (4::Int) [2,3,5]  `shouldBe`  False

  describe "prop_pertenece'" $
    it "p1" $
      property (prop_pertenece :: Int -> [Int] -> Bool)

  describe "concatenaListas" $
    it "e1" $
      concatenaListas [[1..3::Int],[5..7],[8..10]]  `shouldBe`  [1,2,3,5,6,7,8,9,10]

  describe "prop_concat" $
    it "p1" $
      property prop_concat

  describe "coge" $ do
    it "e1" $
      coge 3 [4..12::Int]  `shouldBe`  [4,5,6]
    it "e2" $
      coge (-3) [4..12::Int]  `shouldBe` []

  describe "prop_coge" $
    it "e1" $
      property prop_coge

  describe "sumaCuadradosR" $
    it "e1" $
      sumaCuadradosR 4  `shouldBe`  30

  describe "prop_sumaCuadrados" $
    it "p1" $
      property prop_SumaCuadrados

  describe "sumaCuadradosC" $
    it "e1" $
      sumaCuadradosC 4  `shouldBe`  30

  describe "prop_sumaCuadradosR" $
    it "p1" $
      property prop_sumaCuadradosR

  describe "digitosR" $
    it "e1" $
      digitosR 320274  `shouldBe`  [3,2,0,2,7,4]

  describe "digitosC" $
    it "e1" $
      digitosC 320274  `shouldBe`  [3,2,0,2,7,4]

  describe "prop_digitos" $
    it "p1" $
      property prop_digitos

  describe "sumaDigitosR" $ do
    it "e1" $
      sumaDigitosR 3     `shouldBe`  3
    it "e2" $
      sumaDigitosR 2454  `shouldBe` 15
    it "e3" $
      sumaDigitosR 20045 `shouldBe` 11

  describe "sumaDigitosNR" $ do
    it "e1" $
      sumaDigitosNR 3     `shouldBe`  3
    it "e2" $
      sumaDigitosNR 2454  `shouldBe` 15
    it "e3" $
      sumaDigitosNR 20045 `shouldBe` 11

  describe "prop_sumaDigitos" $
    it "p1" $
      property prop_sumaDigitos

  describe "listaNumeroR" $ do
    it "e1" $
      listaNumeroR [5]        `shouldBe` 5
    it "e2" $
      listaNumeroR [1,3,4,7]  `shouldBe` 1347
    it "e3" $
      listaNumeroR [0,0,1]    `shouldBe` 1

  describe "listaNumeroC" $ do
    it "e1" $
      listaNumeroC [5]        `shouldBe` 5
    it "e2" $
      listaNumeroC [1,3,4,7]  `shouldBe` 1347
    it "e3" $
      listaNumeroC [0,0,1]    `shouldBe` 1

  describe "prop_listaNumero" $
    it "p1" $
      property prop_listaNumero

  describe "mayorExponenteR" $ do
    it "e1" $
      mayorExponenteR 2 8    `shouldBe`  3
    it "e2" $
      mayorExponenteR 2 9    `shouldBe`  0
    it "e3" $
      mayorExponenteR 5 100  `shouldBe`  2
    it "e4" $
      mayorExponenteR 2 60   `shouldBe`  2

  describe "mayorExponenteC" $ do
    it "e1" $
      mayorExponenteC 2 8    `shouldBe`  3
    it "e2" $
      mayorExponenteC 5 100  `shouldBe`  2
    it "e3" $
      mayorExponenteC 5 101  `shouldBe`  0
