-- Ecuacion_con_factoriales.hs
-- Ecuación con factoriales.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Ecuacion_con_factoriales_Spec where

import Ecuacion_con_factoriales
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorial" $ do
    it "e1" $
      factorial  5  `shouldBe`  120
    it "e2" $
      factorial1 5  `shouldBe`  120
    it "e3" $
      factorial2 5  `shouldBe`  120

  describe "factoriales" $ do
    it "e1" $
      take 7 factoriales   `shouldBe`  [1,1,2,6,24,120,720]
    it "e2" $
      take 7 factoriales1  `shouldBe`  [1,1,2,6,24,120,720]
    it "e3" $
      take 7 factoriales2  `shouldBe`  [1,1,2,6,24,120,720]
    it "e4" $
      take 7 factoriales3  `shouldBe`  [1,1,2,6,24,120,720]

  describe "esFactorial" $ do
    it "e1" $
      esFactorial 120  `shouldBe`  True
    it "e2" $
      esFactorial  20  `shouldBe`  False

  describe "posicionesFactoriales" $
    it "e1" $
      take 7 posicionesFactoriales
      `shouldBe` [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720)]

  describe "invFactorial" $ do
    it "e1" $
      invFactorial 120  `shouldBe` Just 5
    it "e2" $
      invFactorial 20   `shouldBe` Nothing

  describe "pares" $
    it "e1" $
      take 11 pares
      `shouldBe` [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),(0,3),(1,3),(2,3),(3,3),(0,4)]

  describe "solucionFactoriales" $
    it "e1" $
      solucionFactoriales `shouldBe` (3,3,4)

  describe "prop_solucionFactoriales" $
    it "e1" $
      property prop_solucionFactoriales
