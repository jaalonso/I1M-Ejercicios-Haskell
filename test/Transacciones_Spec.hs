{-# OPTIONS_GHC -Wno-orphans #-}

module Transacciones_Spec (spec, main) where

import Test.Hspec
import Test.QuickCheck
import Data.Map

import Transacciones

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inversa" $ do
    it "e1" $
      inversa (Transaccion {trImporte = 10, trDe = "Ana", trA = "Luis"})
      `shouldBe` Transaccion {trImporte = -10, trDe = "Luis", trA = "Ana"}
    it "p1" $
      property prop_inversa_inversa
  describe "normalizada" $ do
    it "e1" $
      normalizada (Transaccion {trImporte = -5, trDe = "Ana", trA = "Luis"})
      `shouldBe` Transaccion {trImporte = 5, trDe = "Luis", trA = "Ana"}
    it "e2" $
      normalizada (Transaccion {trImporte = 7, trDe = "Ana", trA = "Luis"})
      `shouldBe` Transaccion {trImporte = 7, trDe = "Ana", trA = "Luis"}
    it "p1" $
      property prop_normalizada_normalizada
    it "p2" $
      property prop_normalizada_no_negativa
  describe "procesaTransaccion" $ do
    it "e1" $
      procesaTransaccion (Transaccion 60 "Ana" "Bea") (fromList [("Ana",90)])
      `shouldBe` fromList [("Ana",30),("Bea",60)]
    it "e2" $
      procesaTransaccion (Transaccion 40 "Bea" "Ana") (fromList [("Ana",30),("Bea",60)])
      `shouldBe` fromList [("Ana",70),("Bea",20)]
    it "e3" $
      procesaTransaccion (Transaccion 40 "Bea" "Eva") (fromList [("Ana",70),("Bea",20)])
      `shouldBe` fromList [("Ana",70),("Bea",-20),("Eva",40)]
    it "e1b" $
      procesaTransaccion2 (Transaccion 60 "Ana" "Bea") (fromList [("Ana",90)])
      `shouldBe` fromList [("Ana",30),("Bea",60)]
    it "e2b" $
      procesaTransaccion2 (Transaccion 40 "Bea" "Ana") (fromList [("Ana",30),("Bea",60)])
      `shouldBe` fromList [("Ana",70),("Bea",20)]
    it "e3b" $
      procesaTransaccion2 (Transaccion 40 "Bea" "Eva") (fromList [("Ana",70),("Bea",20)])
      `shouldBe` fromList [("Ana",70),("Bea",-20),("Eva",40)]
    it "e1c" $
      procesaTransaccion3 (Transaccion 60 "Ana" "Bea") (fromList [("Ana",90)])
      `shouldBe` fromList [("Ana",30),("Bea",60)]
    it "e2c" $
      procesaTransaccion3 (Transaccion 40 "Bea" "Ana") (fromList [("Ana",30),("Bea",60)])
      `shouldBe` fromList [("Ana",70),("Bea",20)]
    it "e3c" $
      procesaTransaccion3 (Transaccion 40 "Bea" "Eva") (fromList [("Ana",70),("Bea",20)])
      `shouldBe` fromList [("Ana",70),("Bea",-20),("Eva",40)]
    it "p1" $
      property prop_procesaTransaccion
  describe "procesaTransacciones" $
    it "e1" $
      let t1 = Transaccion 60 "Ana" "Bea"
          t2 = Transaccion 40 "Bea" "Ana"
          t3 = Transaccion 40 "Bea" "Eva"
      in procesaTransacciones [t1,t2,t3] (fromList [("Ana",90)])
      `shouldBe` fromList [("Ana",70),("Bea",-20),("Eva",40)]
  describe "procesaTransaccion'" $ do
    it "e1" $
      procesaTransaccion' (Transaccion 80 "Ana" "Bea") (fromList [("Ana",70)])
      `shouldBe` Nothing
    it "e2" $
      procesaTransaccion' (Transaccion 60 "Ana" "Bea") (fromList [("Ana",70)])
      `shouldBe` Just (fromList [("Ana",10),("Bea",60)])
    it "e3" $
      procesaTransaccion' (Transaccion 70 "Bea" "Ana") (fromList [("Ana",10),("Bea",60)])
      `shouldBe` Nothing
    it "e4" $
      procesaTransaccion' (Transaccion 40 "Bea" "Ana") (fromList [("Ana",10),("Bea",60)])
      `shouldBe` Just (fromList [("Ana",50),("Bea",20)])
  describe "procesaTransacciones'" $ do
    it "e1" $
      let t1 = Transaccion 60 "Ana" "Bea"
          t2 = Transaccion 40 "Bea" "Ana"
          t3 = Transaccion 40 "Bea" "Eva"
      in procesaTransacciones' [t1,t2,t3] (fromList [("Ana",90)])
      `shouldBe` Nothing
    it "e2" $
      let t1 = Transaccion 60 "Ana" "Bea"
          t2 = Transaccion 40 "Bea" "Ana"
          t3 = Transaccion 40 "Bea" "Eva"
      in procesaTransacciones'' [t1,t2,t3] (fromList [("Ana",90)])
      `shouldBe` Nothing
