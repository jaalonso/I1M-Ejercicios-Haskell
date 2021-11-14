module Programacion_dinamica_Apilamiento_de_barriles_Spec (main, spec) where

import Programacion_dinamica_Apilamiento_de_barriles
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "montonesR" $ do
    it "e1" $
      montonesR 1   `shouldBe`  1
    it "e2" $
      montonesR 5   `shouldBe`  34
    it "e3" $
      montonesR 10  `shouldBe`  4181
    it "e4" $
      montonesR 15  `shouldBe`  514229

  describe "montonesPD" $ do
    it "e1" $
      montonesPD 1   `shouldBe`  1
    it "e2" $
      montonesPD 5   `shouldBe`  34
    it "e3" $
      montonesPD 10  `shouldBe`  4181
    it "e4" $
      montonesPD 15  `shouldBe`  514229

  describe "montonesR2" $ do
    it "e1" $
      montonesR2 1   `shouldBe`  1
    it "e2" $
      montonesR2 5   `shouldBe`  34
    it "e3" $
      montonesR2 10  `shouldBe`  4181
    it "e4" $
      montonesR2 15  `shouldBe`  514229

  describe "montonesPD2" $ do
    it "e1" $
      montonesPD2 1   `shouldBe`  1
    it "e2" $
      montonesPD2 5   `shouldBe`  34
    it "e3" $
      montonesPD2 10  `shouldBe`  4181
    it "e4" $
      montonesPD2 15  `shouldBe`  514229

  describe "sucMontones" $
    it "e1" $
      take 10 sucMontones  `shouldBe`  [1,2,5,13,34,89,233,610,1597,4181]

  describe "montonesS" $ do
    it "e1" $
      montonesS 1   `shouldBe`  1
    it "e2" $
      montonesS 5   `shouldBe`  34
    it "e3" $
      montonesS 10  `shouldBe`  4181
    it "e4" $
      montonesS 15  `shouldBe`  514229
