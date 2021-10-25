module Calculo_numerico_Diferenciacion_y_metodos_de_Heron_y_de_Newton_Spec (main,spec) where

import Calculo_numerico_Diferenciacion_y_metodos_de_Heron_y_de_Newton
import Test.Hspec
import Test.QuickCheck
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "derivada" $ do
    it "e1" $
      derivada 0.001 sin pi  `shouldBe`  -0.9999998333332315
    it "e2" $
      derivada 0.001 cos pi  `shouldBe`  4.999999583255033e-4

  describe "derivadaBurda" $
    it "e1" $
      derivadaBurda cos pi  `shouldBe`  4.999958333473664e-3

  describe "derivadaFina" $
    it "e1" $
      derivadaFina  cos pi  `shouldBe`  4.999999969612645e-5

  describe "derivadaSuper" $
    it "e1" $
      derivadaSuper cos pi  `shouldBe`  5.000444502911705e-7

  describe "derivadaFinaDelSeno" $
    it "e1" $
      derivadaFinaDelSeno pi  `shouldBe`  -0.9999999983354435

  describe "raiz" $
    it "e1" $
      raiz 9  `shouldBe`  3.000000001396984

  describe "(~=)" $ do
    it "e1" $
      3.05 ~= 3.07        `shouldBe`  False
    it "e2" $
      3.00005 ~= 3.00007  `shouldBe` True

  describe "prop_raiz" $
    it "e1" $
      property prop_raiz

  describe "until'" $
    it "e1" $
      until' (>1000) (2*) 1  `shouldBe`  1024

  describe "raizI" $
    it "e1" $
      raizI 9  `shouldBe`  3.000000001396984

  describe "prop_raizI" $
    it "e1" $
      property prop_raizI

  describe "puntoCero" $
    it "e1" $
      puntoCero cos  `shouldBe`  1.5707963267949576

  describe "puntoCeroI" $
    it "e1" $
      puntoCeroI cos  `shouldBe`  1.5707963267949576

  describe "raizCuadrada" $
    it "e1" $
      raizCuadrada 9  `shouldBe`  3.000000002941184

  describe "prop_raizCuadrada" $
    it "e1" $
      property prop_raizCuadrada

  describe "raizCubica" $
    it "e1" $
      raizCubica 27  `shouldBe`  3.0000000000196048

  describe "prop_raizCubica" $
    it "e1" $
      property prop_raizCubica

  describe "arcoseno" $
    it "e1" $
      arcoseno 1  `shouldBe` 1.5665489428306574

  describe "prop_arcoseno2" $ do
    it "e1" $
      property prop_arcoseno
    it "e2" $
      property prop_arcoseno2

  describe "arcocoseno" $
    it "e1" $
      arcocoseno 0  `shouldBe` 1.5707963267949576

  describe "prop_arcocoseno" $ do
    it "e1" $
      property prop_arcocoseno
    it "e2" $
      property prop_arcocoseno2

  describe "inversa" $
    it "e1" $
      inversa (^2) 9  `shouldBe`  3.000000002941184
