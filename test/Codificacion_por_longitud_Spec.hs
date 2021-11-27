module Codificacion_por_longitud_Spec (main, spec) where

import Codificacion_por_longitud
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "comprimida" $ do
    it "e1" $
      comprimida [1,1,7,7,7,5,5,7,7,7,7]
      `shouldBe` [(2,1),(3,7),(2,5),(4,7)]
    it "e2" $
      comprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
      `shouldBe` [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
    it "e3" $
      comprimida ([] :: [Int])
      `shouldBe` []
    it "e4" $
      comprimida2 [1,1,7,7,7,5,5,7,7,7,7]
      `shouldBe` [(2,1),(3,7),(2,5),(4,7)]
    it "e5" $
      comprimida2 "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
      `shouldBe` [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
    it "e6" $
      comprimida2 ([] :: [Int])
      `shouldBe` []
    it "e7" $
      comprimida3 [1,1,7,7,7,5,5,7,7,7,7]
      `shouldBe` [(2,1),(3,7),(2,5),(4,7)]
    it "e8" $
      comprimida3 "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
      `shouldBe` [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
    it "e9" $
      comprimida3 ([] :: [Int])
      `shouldBe` []
    it "e10" $
      comprimida4 [1,1,7,7,7,5,5,7,7,7,7]
      `shouldBe` [(2,1),(3,7),(2,5),(4,7)]
    it "e11" $
      comprimida4 "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
      `shouldBe` [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
    it "e12" $
      comprimida4 ([] :: [Int])
      `shouldBe` []

  describe "expandida" $ do
    it "e1" $
      expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
    it "e2" $
      expandida2 [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
    it "e3" $
      expandida3 [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
    it "e4" $
      expandida4 [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]

  describe "propiedades" $ do
    it "e1" $
      property prop_expandida_comprimida
    it "e2" $
      property prop_comprimida_expandida

  describe "listaAcadena" $
    it "e1" $
      listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
      `shouldBe` "12B1N12B3N19B"

  describe "cadenaComprimida" $
    it "e1" $
      cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
      `shouldBe` "12B1N12B3N10B3N"

  describe "cadenaAlista" $
    it "e1" $
      cadenaAlista "12B1N12B3N10B3N"
      `shouldBe` [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]

  describe "cadenaExpandida" $
    it "e1" $
      cadenaExpandida "12B1N12B3N10B3N"
      `shouldBe` "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
