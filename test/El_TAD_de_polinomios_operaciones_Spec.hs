module El_TAD_de_polinomios_operaciones_Spec (main, spec) where

import El_TAD_de_polinomios_operaciones
import I1M.PolOperaciones
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "creaPolDispersa" $
    it "e1" $
      show (creaPolDispersa [7,0,0,4,0,3])
      `shouldBe` "7*x^5 + 4*x^2 + 3"

  describe "creaPolDensa" $ do
    it "e1" $
      show (creaPolDensa  [(5,7),(4,2),(3,0)]) `shouldBe` "7*x^5 + 2*x^4"
    it "e2" $
      show (creaPolDensa2 [(5,7),(4,2),(3,0)]) `shouldBe` "7*x^5 + 2*x^4"
    it "e3" $
      show (creaPolDensa3 [(5,7),(4,2),(3,0)]) `shouldBe` "7*x^5 + 2*x^4"

  describe "densa" $
    it "e1" $
      densa pol1  `shouldBe`  [(5,1),(2,5),(1,4)]

  describe "densaAdispersa" $
    it "e1" $
      densaAdispersa [(5,1),(2,5),(1,4)]  `shouldBe`  [1,0,0,5,4,0]

  describe "dispersa" $
    it "e1" $
      dispersa pol1  `shouldBe`  [1,0,0,5,4,0]

  describe "coeficiente" $ do
    it "e1" $
      coeficiente  2 pol1  `shouldBe`  5
    it "e2" $
      coeficiente  3 pol1  `shouldBe`  0
    it "e3" $
      coeficiente' 2 pol1  `shouldBe`  5
    it "e4" $
      coeficiente' 3 pol1  `shouldBe`  0

  describe "coeficientes" $ do
    it "e1" $
      coeficientes  pol1  `shouldBe`  [1,0,0,5,4,0]
    it "e2" $
      coeficientes2 pol1  `shouldBe`  [1,0,0,5,4,0]

  describe "potencia" $ do
    it "e1" $
      show (potencia pol2 2) `shouldBe` "4*x^2 + 12*x + 9"
    it "e2" $
      show (potencia pol2 3) `shouldBe` "8*x^3 + 36*x^2 + 54*x + 27"

  describe "potenciaM" $ do
    it "e1" $
      show (potenciaM pol2 2) `shouldBe` "4*x^2 + 12*x + 9"
    it "e2" $
      show (potenciaM pol2 3) `shouldBe` "8*x^3 + 36*x^2 + 54*x + 27"

  describe "integral" $ do
    it "e1" $
      show (integral pol3) `shouldBe` "0.25*x^8 + x^5 + 1.6666666666666667*x^3"
    it "e2" $
      show (integral pol3 :: Polinomio Rational)
      `shouldBe` "1 % 4*x^8 + x^5 + 5 % 3*x^3"

  describe "integralDef" $ do
    it "e1" $
      integralDef pol3 0 1 `shouldBe` 2.916666666666667
    it "e2" $
      (integralDef pol3 0 1 :: Rational) `shouldBe` 35 % 12

  describe "multEscalar" $ do
    it "e1" $
      show (multEscalar 4 pol2) `shouldBe` "8*x + 12"
    it "e2" $
      show (multEscalar (1%4) pol2) `shouldBe` "1 % 2*x + 3 % 4"

  describe "cociente" $
    it "e1" $
      show (cociente pol4 pol5) `shouldBe` "1 % 2*x^2 + (-1) % 6*x + 8 % 9"

  describe "resto" $
    it "e1" $
      show (resto pol4 pol5) `shouldBe` "(-16) % 9*x + 3 % 1"

  describe "divisiblePol" $ do
    it "e1" $
      divisiblePol pol6 pol2  `shouldBe`  True
    it "e2" $
      divisiblePol pol6 pol5  `shouldBe`  False

  describe "horner" $ do
    it "e1" $
      horner  pol1 0     `shouldBe`  0
    it "e2" $
      horner  pol1 1     `shouldBe`  10
    it "e3" $
      horner  pol1 1.5   `shouldBe`  24.84375
    it "e4" $
      horner  pol1 (3%2) `shouldBe`  795 % 32
    it "e5" $
      horner2 pol1 0     `shouldBe`  0
    it "e6" $
      horner2 pol1 1     `shouldBe`  10
    it "e7" $
      horner2 pol1 1.5   `shouldBe`  24.84375
    it "e8" $
      horner2 pol1 (3%2) `shouldBe`  795 % 32
