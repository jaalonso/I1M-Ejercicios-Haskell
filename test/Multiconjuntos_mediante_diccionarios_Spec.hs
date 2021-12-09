module Multiconjuntos_mediante_diccionarios_Spec (main, spec) where

import Multiconjuntos_mediante_diccionarios
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "vacio" $
    it "e1" $
      show (vacio :: MultiConj Int)  `shouldBe`  "fromList []"

  describe "unitario" $
    it "e1" $
      show (unitario 'a')  `shouldBe`  "fromList [('a',1)]"

  describe "inserta" $
    it "e1" $
      show (inserta 'a' (unitario 'a')) `shouldBe` "fromList [('a',2)]"

  describe "listaAmc" $ do
    it "e1" $
      show (listaAmc "ababc")  `shouldBe`  "fromList [('a',2),('b',2),('c',1)]"
    it "e2" $
      show (listaAmc2 "ababc")  `shouldBe`  "fromList [('a',2),('b',2),('c',1)]"

  describe "insertaVarios" $ do
    it "e1" $
      show (insertaVarios 'a' 3 vacio )`shouldBe` "fromList [('a',3)]"
    it "e2" $
      show (insertaVarios2 'a' 3 vacio )`shouldBe` "fromList [('a',3)]"

  describe "borra" $
    it "e1" $
      show (borra 'a' (listaAmc "ababc")) `shouldBe` "fromList [('a',1),('b',2),('c',1)]"

  describe "borraVarias" $ do
    it "e1" $
      show (borraVarias 'a' 2 (listaAmc "ababcad")) `shouldBe` "fromList [('a',1),('b',2),('c',1),('d',1)]"
    it "e2" $
      show (borraVarias2 'a' 2 (listaAmc "ababcad")) `shouldBe` "fromList [('a',1),('b',2),('c',1),('d',1)]"

  describe "borraTodas" $
    it "e1" $
      show (borraTodas 'a' (listaAmc "ababcad")) `shouldBe` "fromList [('b',2),('c',1),('d',1)]"

  describe "esVacio" $ do
    it "e1" $
      esVacio vacio  `shouldBe`  True
    it "e2" $
      esVacio (inserta 'a' vacio)  `shouldBe`  False

  describe "cardinal" $ do
    it "e1" $
      cardinal (listaAmc "ababcad")  `shouldBe`  7
    it "e2" $
      cardinal2 (listaAmc "ababcad")  `shouldBe`  7

  describe "cardDistintos" $ do
    it "e1" $
      cardDistintos (listaAmc "ababcad")  `shouldBe`  4
    it "e2" $
      cardDistintos2 (listaAmc "ababcad")  `shouldBe`  4

  describe "pertenece" $ do
    it "e1" $
      pertenece 'b' (listaAmc "ababcad")  `shouldBe`  True
    it "e2" $
      pertenece 'r' (listaAmc "ababcad")  `shouldBe`  False

  describe "noPertenece" $ do
    it "e1" $
      noPertenece 'b' (listaAmc "ababcad")  `shouldBe`  False
    it "e2" $
      noPertenece 'r' (listaAmc "ababcad")  `shouldBe`  True

  describe "ocurrencias" $ do
    it "e1" $
      ocurrencias 'a' (listaAmc "ababcad")  `shouldBe`  3
    it "e2" $
      ocurrencias 'r' (listaAmc "ababcad")  `shouldBe`  0

  describe "elementos" $
    it "e1" $
      elementos (listaAmc "ababcad")  `shouldBe`  "abcd"

  describe "esSubmultiConj" $ do
    it "e1" $
      esSubmultiConj (listaAmc "ababcad") (listaAmc "bcbaadaa")
      `shouldBe` True
    it "e2" $
      esSubmultiConj (listaAmc "bcbaadaa") (listaAmc "ababcad")
      `shouldBe` False
    it "e3" $
      esSubmultiConj2 (listaAmc "ababcad") (listaAmc "bcbaadaa")
      `shouldBe` True
    it "e4" $
      esSubmultiConj2 (listaAmc "bcbaadaa") (listaAmc "ababcad")
      `shouldBe` False

  describe "minimo" $
    it "e1" $
      minimo (listaAmc "cdacbab")  `shouldBe`  'a'

  describe "maximo" $
    it "e1" $
      maximo (listaAmc "cdacbab")  `shouldBe`  'd'

  describe "borraMin" $
    it "e1" $
      show (borraMin (listaAmc "cdacbab")) `shouldBe` "fromList [('a',1),('b',2),('c',2),('d',1)]"

  describe "borraMax" $
    it "e1" $
      show (borraMax (listaAmc "cdacbab")) `shouldBe` "fromList [('a',2),('b',2),('c',2)]"

  describe "borraMinTodo" $
    it "e1" $
      show (borraMinTodo (listaAmc "cdacbab")) `shouldBe` "fromList [('b',2),('c',2),('d',1)]"

  describe "borraMaxTodo" $
    it "e1" $
      show (borraMaxTodo (listaAmc "cdacbab")) `shouldBe` "fromList [('a',2),('b',2),('c',2)]"

  describe "union" $
    it "e1" $
      show (union (listaAmc "cdacba") (listaAmc "acec"))
      `shouldBe` "fromList [('a',3),('b',1),('c',4),('d',1),('e',1)]"

  describe "unionG" $ do
    it "e1" $
      show (unionG (map listaAmc ["aba", "cda", "bdb"]))
      `shouldBe` "fromList [('a',3),('b',3),('c',1),('d',2)]"
    it "e2" $
      show (unionG2 (map listaAmc ["aba", "cda", "bdb"]))
      `shouldBe` "fromList [('a',3),('b',3),('c',1),('d',2)]"

  describe "diferencia" $
    it "e1" $
      show (diferencia (listaAmc "abacc") (listaAmc "dcb"))
      `shouldBe` "fromList [('a',2),('c',1)]"

  describe "interseccion" $
    it "e1" $
      show (interseccion (listaAmc "abcacc") (listaAmc "bdcbc"))
      `shouldBe` "fromList [('b',1),('c',2)]"

  describe "filtra" $
    it "e1" $
      show (filtra (>'b') (listaAmc "abaccaded"))
      `shouldBe` "fromList [('c',2),('d',2),('e',1)]"

  describe "particion" $
    it "e1" $
      show (particion (>'b') (listaAmc "abaccaded"))
      `shouldBe` "(fromList [('c',2),('d',2),('e',1)],fromList [('a',3),('b',1)])"

  describe "mapMC" $
    it "e1" $
      show (mapMC (:"N") (listaAmc "abaccaded"))
      `shouldBe` "fromList [(\"aN\",3),(\"bN\",1),(\"cN\",2),(\"dN\",2),(\"eN\",1)]"
