{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Calculo_numerico_2_Limites_biseccion_e_integrales_Spec (main, spec) where

import Calculo_numerico_2_Limites_biseccion_e_integrales
import Test.QuickCheck
import Data.Matrix
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "limite" $ do
    it "e1" $
      limite (\n -> (2*n+1)/(n+5)) 0.001  `shouldBe`  1.9900110987791344
    it "e2" $
      limite (\n -> (1+1/n)**n) 0.001     `shouldBe`  2.714072874546881

  describe "biseccion" $ do
    it "e1" $
      biseccion (\x -> x^2 - 3) 0 5 0.01             `shouldBe`  1.7333984375
    it "e2" $
      biseccion (\x -> x^3 - x - 2) 0 4 0.01         `shouldBe`  1.521484375
    it "e3" $
      biseccion cos 0 2 0.01                         `shouldBe`  1.5625
    it "e4" $
      biseccion (\x -> log (50-x) - 4) (-10) 3 0.01  `shouldBe`  -5.125

  describe "raizEnt" $ do
    it "e1" $
      raizEnt1  8 3      `shouldBe`  2
    it "e2" $
      raizEnt1  9 3      `shouldBe`  2
    it "e3" $
      raizEnt1 26 3      `shouldBe`  2
    it "e4" $
      raizEnt1 27 3      `shouldBe`  3
    it "e8" $
      raizEnt2 (10^14) 2 `shouldBe` 10000000
    it "e9" $
      raizEnt3 (10^14) 2 `shouldBe` 10000000
    it "e12" $
      property prop_raizEnt

  describe "integral" $ do
    it "e1" $
      integral 0 1 (^3) 0.01  `shouldBe`  0.24998750000000042
    it "e2" $
      integral 0 1 (^4) 0.01                        `shouldBe`  0.19998333362500048
    it "e3" $
      integral 0 1 (\x -> 3*x^2 + 4*x^3) 0.01       `shouldBe`  1.9999250000000026
    it "e4" $
      log 2 - integral 1 2 (\x -> 1/x) 0.01         `shouldBe`  3.124931644782336e-6
    it "e5" $
      pi - 4 * integral 0 1 (\x -> 1/(x^2+1)) 0.01  `shouldBe`  -8.333333331389525e-6

  describe "suma" $
    it "e1" $
      suma 2 5 (1+) (^3)  `shouldBe`  224

  describe "sucesion" $ do
    it "e1" $
      sucesion 3 20 (+2)  `shouldBe`  [3,5,7,9,11,13,15,17,19]

  describe "bajada" $
    it "e1" $
      let a = fromLists [[2,0,0],[3,1,0],[4,2,5.0]]
          b = fromLists [[3],[6.5],[10]]
      in bajada a b
      `shouldBe` fromLists [[1.5],
                            [2.0],
                            [0.0]]
