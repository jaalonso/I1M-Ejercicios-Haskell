module Definiciones_por_comprension_con_cadenas_El_cifrado_Cesar_Spec (main, spec) where

import Definiciones_por_comprension_con_cadenas_El_cifrado_Cesar
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minuscula2int" $ do
    it "e1" $
      minuscula2int 'a'  `shouldBe`  0
    it "e1" $
      minuscula2int 'd'  `shouldBe`  3
    it "e1" $
      minuscula2int 'z'  `shouldBe`  25

  describe "mayuscula2int" $ do
    it "e1" $
      mayuscula2int 'A'  `shouldBe`  0
    it "e2" $
      mayuscula2int 'D'  `shouldBe`  3
    it "e3" $
      mayuscula2int 'Z'  `shouldBe`  25

  describe "int2minuscula" $ do
    it "e1" $
      int2minuscula 0   `shouldBe`  'a'
    it "e2" $
      int2minuscula 3   `shouldBe`  'd'
    it "e3" $
      int2minuscula 25  `shouldBe`  'z'

  describe "int2mayuscula" $ do
    it "e1" $
      int2mayuscula 0   `shouldBe`  'A'
    it "e2" $
      int2mayuscula 3   `shouldBe`  'D'
    it "e3" $
      int2mayuscula 25  `shouldBe`  'Z'

  describe "desplaza" $ do
    it "e1" $
      desplaza   3  'a'  `shouldBe`  'd'
    it "e2" $
      desplaza   3  'y'  `shouldBe`  'b'
    it "e3" $
      desplaza (-3) 'd'  `shouldBe`  'a'
    it "e4" $
      desplaza (-3) 'b'  `shouldBe`  'y'
    it "e5" $
      desplaza   3  'A'  `shouldBe`  'D'
    it "e6" $
      desplaza   3  'Y'  `shouldBe`  'B'
    it "e7" $
      desplaza (-3) 'D'  `shouldBe`  'A'
    it "e8" $
      desplaza (-3) 'B'  `shouldBe`  'Y'

  describe "codifica" $ do
    it "e1" $
      codifica   3  "En Todo La Medida"
      `shouldBe` "Hq Wrgr Od Phglgd"
    it "e2" $
      codifica (-3) "Hq Wrgr Od Phglgd"
      `shouldBe` "En Todo La Medida"

  describe "prop_codifica" $
    it "e1" $
      property prop_codifica

  describe "porcentaje" $
    it "e1" $
      porcentaje 2 5  `shouldBe`  40.0

  describe "letras" $
    it "e1" $
      letras "Esto Es Una Prueba"  `shouldBe`  "EstoEsUnaPrueba"

  describe "ocurrencias" $
    it "e1" $
      ocurrencias 'a' "Salamanca"  `shouldBe`  4

  describe "prop_ocurrencia_inv" $
    it "e1" $
      property prop_ocurrencia_inv

  describe "prop_ocurrencia_conc" $
    it "e1" $
      property prop_ocurrencia_conc

  describe "frecuencias" $
    it "e1" $
      frecuencias "En Todo La Medida" `shouldBe`
      [14.285715,0.0,0.0,21.428572,14.285715,0.0,0.0,0.0,7.1428576,0.0,0.0,7.1428576,7.1428576,7.1428576,14.285715,0.0,0.0,0.0,0.0,7.1428576,0.0,0.0,0.0,0.0,0.0,0.0]

  describe "chiCuad" $ do
    it "e1" $
      chiCuad [3,5,6] [3,5,6]  `shouldBe`  0.0
    it "e2" $
      chiCuad [3,5,6] [5,6,3]  `shouldBe`  3.9666667

  describe "prop_chiCuad_3" $
    it "e1" $
      property prop_chiCuad_3

  describe "rota" $ do
    it "e1" $
      rota  2 "manolo"              `shouldBe`  "noloma"
    it "e2" $
      rota 10 "manolo"              `shouldBe`  "lomano"
    it "e3" $
      [rota n "abc" | n <- [0..5]]  `shouldBe`  ["abc","bca","cab","abc","bca","cab"]

  describe "descifra" $ do
    it "e1" $
      codifica 5 "Todo Para Nada"
      `shouldBe` "Ytit Ufwf Sfif"
    it "e2" $
      descifra "Ytit Ufwf Sfif"
      `shouldBe` "Todo Para Nada"
