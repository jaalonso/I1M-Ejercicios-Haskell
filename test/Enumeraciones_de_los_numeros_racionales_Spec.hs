module Enumeraciones_de_los_numeros_racionales_Spec (main, spec) where

import Enumeraciones_de_los_numeros_racionales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "potenciasDeDos" $
    it "e1" $
      take 10 potenciasDeDos  `shouldBe`  [1,2,4,8,16,32,64,128,256,512]

  describe "empiezaConDos" $ do
    it "e1" $
      empiezaConDos 5 [5,5,3,7]  `shouldBe`  True
    it "e2" $
      empiezaConDos 5 [5,3,5,7]  `shouldBe`  False
    it "e3" $
      empiezaConDos 5 [5,5,5,7]  `shouldBe`  True

  describe "representacionesHB" $ do
    it "e1" $
      representacionesHB 5  `shouldBe`  [[1,2,2],[1,4]]
    it "e2" $
      representacionesHB 6  `shouldBe`  [[1,1,2,2],[1,1,4],[2,4]]

  describe "nRepresentacionesHB" $
    it "e1" $
      [nRepresentacionesHB n | n <- [0..20]]
      `shouldBe` [1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8]

  describe "termino" $
    it "e1" $
      termino 4  `shouldBe`  (3,2)

  describe "sucesionHB" $
    it "e1" $
      take 10 sucesionHB
      `shouldBe` [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]

  describe "contenido" $
    it "e1" $
      contenido 5  `shouldBe`  True

  describe "indice" $
    it "e1" $
      indice (3,2)  `shouldBe`  4

  describe "sucesores" $
    it "e1" $
      sucesores (3,2)  `shouldBe`  [(3,5),(5,2)]

  describe "siguiente" $
    it "e1" $
      siguiente [(1,3),(3,2),(2,3),(3,1)]
      `shouldBe` [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]

  describe "nivelesCalkinWilf" $
    it "e1" $
      take 4 nivelesCalkinWilf `shouldBe`
      [[(1,1)],
       [(1,2),(2,1)],
       [(1,3),(3,2),(2,3),(3,1)],
       [(1,4),(4,3),(3,5),(5,2),(2,5),(5,3),(3,4),(4,1)]]

  describe "sucesionCalkinWilf" $
    it "e1" $
      take 10 sucesionCalkinWilf
      `shouldBe` [(1,1),(1,2),(2,1),(1,3),(3,2),(2,3),(3,1),(1,4),(4,3),(3,5)]

  describe "igual_sucesion_HB_CalkinWilf" $
    it "e1" $
      igual_sucesion_HB_CalkinWilf 20  `shouldBe`  True

  describe "fusc 4" $
    it "e1" $
      fusc 4  `shouldBe`  3
