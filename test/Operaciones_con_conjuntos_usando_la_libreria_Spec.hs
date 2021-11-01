module Operaciones_con_conjuntos_usando_la_libreria_Spec (main, spec) where

import Operaciones_con_conjuntos_usando_la_libreria
import Data.Set as S
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subconjunto" $ do
    it "e1" $
      subconjunto (fromList [2..100000]) (fromList [1..100000]) `shouldBe` True
    it "e2" $
      subconjunto (fromList [1..100000]) (fromList [2..100000]) `shouldBe` False
    it "e3" $
      subconjuntoPropio (fromList [2..5]) (fromList [1..7]) `shouldBe` True
  describe "subconjuntoPropio" $ do
    it "e1" $
      subconjuntoPropio (fromList [2..5]) (fromList [1..4]) `shouldBe` False
    it "e2" $
      subconjuntoPropio (fromList [2..5]) (fromList [2..5]) `shouldBe` False
  describe "unitario" $
    it "e1" $
      show (unitario 5) `shouldBe` "fromList [5]"
  describe "cardinal" $
    it "e1" $
      cardinal (fromList [3,2,5,1,2,3])  `shouldBe`  4
  describe "union" $
    it "e1" $
      show (union' (fromList [3,2,5]) (fromList [2,7,5]))
      `shouldBe` "fromList [2,3,5,7]"
  describe "unionG" $
    it "e1" $
      show (unionG [fromList [3,2], fromList [2,5], fromList [3,5,7]])
      `shouldBe` "fromList [2,3,5,7]"
  describe "interseccion" $ do
    it "e1" $
      show (interseccion (fromList [1..7]) (fromList [4..9]))
      `shouldBe` "fromList [4,5,6,7]"
    it "e2" $
      show (interseccion (fromList [2..1000000]) (fromList [1]))
      `shouldBe` "fromList []"
  describe "interseccionG" $ do
    it "e1" $
      show (interseccionG [fromList [3,2], fromList [2,5,3], fromList [3,5,7]])
      `shouldBe` "fromList [3]"
    it "e2" $
      show (interseccionG2 [fromList [3,2], fromList [2,5,3], fromList [3,5,7]])
      `shouldBe` "fromList [3]"
  describe "disjuntos" $ do
    it "e1" $
      disjuntos (fromList [2..5]) (fromList [6..9]) `shouldBe` True
    it "e2" $
      disjuntos (fromList [2..5]) (fromList [1..9]) `shouldBe` False
  describe "diferencia" $
    it "e1" $
      show (diferencia (fromList [2,5,3]) (fromList [1,4,5]))
      `shouldBe` "fromList [2,3]"
  describe "diferenciaSimetrica" $
    it "e1" $
      show (diferenciaSimetrica (fromList [3,2,5]) (fromList [1,5]))
      `shouldBe` "fromList [1,2,3]"
  describe "filtra" $ do
    it "e1" $
      show (filtra even (fromList [3,2,5,6,8,9]))  `shouldBe`  "fromList [2,6,8]"
    it "e2" $
      show (filtra odd  (fromList [3,2,5,6,8,9]))  `shouldBe`  "fromList [3,5,9]"
  describe "particion" $
    it "e1" $
      show (particion even (fromList [3,2,5,6,8,9]))
      `shouldBe` "(fromList [2,6,8],fromList [3,5,9])"
  describe "divide" $
    it "e1" $
      show (divide 5 (fromList [3,2,9,5,8,6]))
      `shouldBe` "(fromList [2,3],fromList [6,8,9])"
  describe "mapC" $
    it "e1" $
      show (mapC (*2) (fromList [1..4]))  `shouldBe`  "fromList [2,4,6,8]"
  describe "everyC" $ do
    it "e1" $
      everyC even (fromList [2,4..10]) `shouldBe` True
    it "e2" $
      everyC even (fromList [2..10])   `shouldBe` False
    it "e3" $
      everyC2 even (fromList [2,4..10]) `shouldBe` True
    it "e4" $
      everyC2 even (fromList [2..10])   `shouldBe` False
  describe "someC" $ do
    it "e1" $
      someC even (fromList [1,4,7]) `shouldBe` True
    it "e2" $
      someC even (fromList [1,3,7]) `shouldBe` False
    it "e3" $
      someC2 even (fromList [1,4,7]) `shouldBe` True
    it "e4" $
      someC2 even (fromList [1,3,7]) `shouldBe` False
  describe "productoC" $
    it "e1" $
      show (productoC (fromList [1,3]) (fromList [2,4]))
      `shouldBe` "fromList [(1,2),(1,4),(3,2),(3,4)]"
  describe "potencia" $
    it "e1" $
      show (potencia (fromList [1..3]))
      `shouldBe` "fromList [fromList [],fromList [1],fromList [1,2],fromList [1,2,3],fromList [1,3],fromList [2],fromList [2,3],fromList [3]]"
