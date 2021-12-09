module Modelizacion_de_juego_de_cartas_Spec (main, spec) where

import Modelizacion_de_juego_de_cartas
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "color" $
    it "e1" $
      color Corazones `shouldBe`  Rojo

  describe "mayor" $ do
    it "e1" $
      mayor Sota (Numerico 7)    `shouldBe`  True
    it "e2" $
      mayor (Numerico 10) Reina  `shouldBe`  False

  describe "prop_MayorValor" $
    it "e1" $
      property prop_MayorValor

  describe "valor" $
    it "e1" $
      valor (Carta Rey Corazones)  `shouldBe`  Rey

  describe "palo" $
    it "e1" $
      palo (Carta Rey Corazones)  `shouldBe`  Corazones

  describe "ganaCarta" $ do
    it "e1" $
      ganaCarta Corazones (Carta Sota Picas) (Carta (Numerico 5) Picas)
      `shouldBe` True
    it "e2" $
      ganaCarta Corazones (Carta (Numerico 3) Picas) (Carta Sota Picas)
      `shouldBe` False
    it "e3" $
      ganaCarta Corazones (Carta (Numerico 3) Corazones) (Carta Sota Picas)
      `shouldBe` True
    it "e4" $
      ganaCarta Treboles (Carta (Numerico 3) Corazones) (Carta Sota Picas)
      `shouldBe` False

  describe "ganaMano" $ do
    it "e1" $
      ganaMano Picas (Agrega (Carta Sota Picas) Vacia) (Carta Rey Corazones)
      `shouldBe`  True
    it "e2" $
      ganaMano Picas (Agrega (Carta Sota Picas) Vacia) (Carta Rey Picas)
      `shouldBe`  False
