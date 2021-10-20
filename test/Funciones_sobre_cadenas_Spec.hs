module Funciones_sobre_cadenas_Spec (main, spec) where

import Funciones_sobre_cadenas
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sumaDigitosC" $
    it "e1" $
      sumaDigitosC "SE 2431 X"  `shouldBe`  10

  describe "sumaDigitosR" $
    it "e1" $
      sumaDigitosR "SE 2431 X"  `shouldBe`  10

  describe "prop_sumaDigitosC" $
    it "p1" $
      property prop_sumaDigitosC

  describe "mayusculaInicial" $ do
    it "e1" $
      mayusculaInicial "sEviLLa"  `shouldBe`  "Sevilla"
    it "e2" $
      mayusculaInicial ""         `shouldBe`  ""

  describe "mayusculaInicialRec" $ do
    it "e1" $
      mayusculaInicialRec "sEviLLa"  `shouldBe`  "Sevilla"
    it "e2" $
      mayusculaInicialRec "s"  `shouldBe`  "S"

  describe "prop_mayusculaInicial" $
    it "e1" $
      property prop_mayusculaInicial

  describe "titulo" $
    it "e1" $
      titulo ["eL","arTE","DE","La","proGraMacion"]
      `shouldBe` ["El","Arte","de","la","Programacion"]

  describe "tituloRec" $
    it "e1" $
      tituloRec ["eL","arTE","DE","La","proGraMacion"]
      `shouldBe` ["El","Arte","de","la","Programacion"]

  describe "prop_titulo" $
    it "e1" $
      property prop_titulo

  describe "posiciones" $
    it "e1" $
      posiciones "Salamamca" 'a'  `shouldBe`  [1,3,5,8]

  describe "posicionesR" $
    it "e1" $
      posicionesR "Salamamca" 'a'  `shouldBe`  [1,3,5,8]

  describe "prop_posiciones" $
    it "e1" $
      property prop_posiciones

  describe "contieneR" $ do
    it "e1" $
      contieneR "escasamente" "casa"   `shouldBe`  True
    it "e2" $
      contieneR "escasamente" "cante"  `shouldBe`  False
    it "e3" $
      contieneR "" ""                  `shouldBe`  True

  describe "contiene" $ do
    it "e1" $
      contiene "escasamente" "casa"      `shouldBe`  True
    it "e2" $
      contiene "escasamente" "cante"     `shouldBe`  False
    it "e3" $
      contiene "casado y casada" "casa"  `shouldBe`  True
    it "e4" $
      contiene "" ""                     `shouldBe`  True
    prop "p1" $
      \xs ys -> contiene xs ys == contiene2 xs ys

  describe "sufijos" $
    it "e1" $
      sufijos "abc"  `shouldBe`  ["abc","bc","c",""]

  describe "prop_contiene" $
    it "e1" $
      property prop_contiene
