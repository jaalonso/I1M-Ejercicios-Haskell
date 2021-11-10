module Division_y_factorizacion_de_polinomios_Spec (main, spec) where

import Division_y_factorizacion_de_polinomios
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "divisores" $ do
    it "e1" $
      divisores 4 `shouldBe` [1,-1,2,-2,4,-4]
    it "e2" $
      divisores (-6) `shouldBe` [1,-1,2,-2,3,-3,6,-6]

  describe "coeficiente" $ do
    it "e1" $
      coeficiente 4 ejPol1 `shouldBe` 3
    it "e2" $
      coeficiente 3 ejPol1 `shouldBe` 0
    it "e3" $
      coeficiente 2 ejPol1 `shouldBe` (-5)
    it "e4" $
      coeficiente 5 ejPol1 `shouldBe` 0

  describe "terminoIndep" $ do
    it "e1" $
      terminoIndep ejPol1 `shouldBe` 3
    it "e2" $
      terminoIndep ejPol2 `shouldBe` 0
    it "e3" $
      terminoIndep ejPol4 `shouldBe` (-2)

  describe "coeficientes" $ do
    it "e1" $
      coeficientes ejPol1 `shouldBe` [3,0,-5,0,3]
    it "e2" $
      coeficientes ejPol4 `shouldBe` [1,2,-1,-2]
    it "e3" $
      coeficientes ejPol2 `shouldBe` [1,0,0,5,4,0]

  describe "creaPol" $ do
    it "e1" $
      show (creaPol [1,0,0,5,4,0]) `shouldBe` "x^5 + 5*x^2 + 4*x"
    it "e2" $
      show (creaPol [1,2,0,3,0]) `shouldBe` "x^4 + 2*x^3 + 3*x"

  describe "prop_coef" $
    it "e1" $
      property prop_coef

  describe "pRuffini" $ do
    it "e1" $
      pRuffini  2 [1,2,-1,-2] `shouldBe` [1,4,7,12]
    it "e2" $
      pRuffini  1 [1,2,-1,-2] `shouldBe` [1,3,2,0]
    it "e3" $
      pRuffini2 2 [1,2,-1,-2] `shouldBe` [1,4,7,12]
    it "e4" $
      pRuffini2 1 [1,2,-1,-2] `shouldBe` [1,3,2,0]

  describe "cocienteRuffini" $ do
    it "e1" $
      show (cocienteRuffini  2 ejPol4) `shouldBe` "x^2 + 4*x + 7"
    it "e2" $
      show (cocienteRuffini  (-2) ejPol4) `shouldBe` "x^2 + -1"
    it "e3" $
      show (cocienteRuffini  3 ejPol4) `shouldBe` "x^2 + 5*x + 14"
    it "e4" $
      show (cocienteRuffini2 2 ejPol4) `shouldBe` "x^2 + 4*x + 7"
    it "e5" $
      show (cocienteRuffini2 (-2) ejPol4) `shouldBe` "x^2 + -1"
    it "e6" $
      show (cocienteRuffini2 3 ejPol4) `shouldBe` "x^2 + 5*x + 14"

  describe "restoRuffini" $ do
    it "e1" $
      restoRuffini  2 ejPol4    `shouldBe` 12
    it "e2" $
      restoRuffini  (-2) ejPol4 `shouldBe` 0
    it "e3" $
      restoRuffini  3 ejPol4    `shouldBe` 40
    it "e4" $
      restoRuffini2 2 ejPol4    `shouldBe` 12
    it "e5" $
      restoRuffini2 (-2) ejPol4 `shouldBe` 0
    it "e6" $
      restoRuffini2 3 ejPol4    `shouldBe` 40

  describe "prop_diviEuclidea" $
    it "e1" $
      property prop_diviEuclidea

  describe "esRaizRuffini" $ do
    it "e1" $
      esRaizRuffini 0 ejPol3 `shouldBe` True
    it "e2" $
      esRaizRuffini 1 ejPol3 `shouldBe` False

  describe "raicesRuffini" $ do
    it "e1" $
      raicesRuffini ejPol1              `shouldBe` []
    it "e2" $
      raicesRuffini ejPol2              `shouldBe` [0,-1]
    it "e3" $
      raicesRuffini ejPol3              `shouldBe` [0]
    it "e4" $
      raicesRuffini ejPol4              `shouldBe` [1,-1,-2]
    it "e5" $
      raicesRuffini (creaPol [1,-2,1])  `shouldBe` [1,1]

  describe "factorizacion" $ do
    it "e1" $
      show (factorizacion ejPol2) `shouldBe` "[1*x,1*x + 1,x^3 + -1*x^2 + 1*x + 4]"
    it "e2" $
      show (factorizacion ejPol4) `shouldBe` "[1*x + -1,1*x + 1,1*x + 2,1]"
    it "e3" $
      show (factorizacion (creaPol [1,0,0,0,-1])) `shouldBe` "[1*x + -1,1*x + 1,x^2 + 1]"
