module Analizadores_sintacticos_Spec (main, spec) where

import Analizadores_sintacticos
import I1M.Analizador
import Control.Exception (evaluate)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "int" $ do
    it "e1" $
      analiza int "14DeAbril"   `shouldBe`  [(14,"DeAbril")]
    it "e2" $
      analiza int "-14DeAbril"  `shouldBe`  [(-14,"DeAbril")]

  describe "comentario" $ do
    it "e1" $
      analiza comentario "-- 14DeAbril\nSiguiente"
      `shouldBe` [((),"Siguiente")]
    it "e2" $
      analiza comentario "- 14DeAbril\nSiguiente"
      `shouldBe` []

  describe "expr1" $ do
    it "e1" $
      analiza expr1 "2*3+5"     `shouldBe`  [(11,"")]
    it "e2" $
      analiza expr1 "2*(3+5)"   `shouldBe`  [(16,"")]
    it "e3" $
      analiza expr1 "2+3*5"     `shouldBe`  [(17,"")]
    it "e4" $
      analiza expr1 "2*3+5abc"  `shouldBe`  [(11,"abc")]
    it "e5" $
      analiza expr1 "24/4-2"    `shouldBe`  [(4,"")]
    it "e6" $
      analiza expr1 "24/(4-2)"  `shouldBe`  [(12,"")]
    it "e7" $
      analiza expr1 "24-(4/2)"  `shouldBe`  [(22,"")]
    it "e8" $
      analiza expr1 "24/4-2abc" `shouldBe`  [(4,"abc")]

  describe "expr2" $ do
    it "e1" $
      analiza expr2 "2*3+5"     `shouldBe`  [(11,"")]
    it "e2" $
      analiza expr2 "2*(3+5)"   `shouldBe`  [(16,"")]
    it "e3" $
      analiza expr2 "2+3*5"     `shouldBe`  [(17,"")]
    it "e4" $
      analiza expr2 "2*3+5abc"  `shouldBe`  [(11,"abc")]
    it "e5" $
      analiza expr2 "24/4-2"    `shouldBe`  [(4,"")]
    it "e6" $
      analiza expr2 "24/(4-2)"  `shouldBe`  [(12,"")]
    it "e7" $
      analiza expr2 "24-(4/2)"  `shouldBe`  [(22,"")]
    it "e8" $
      analiza expr2 "24/4-2abc" `shouldBe`  [(4,"abc")]
    it "e9" $
      analiza expr2 "2^3*4"     `shouldBe` [(32,"")]

  describe "expr3" $ do
    it "e1" $
      show (analiza expr3 "2*3+5")
      `shouldBe` "[(N '+' (N '*' (H 2) (H 3)) (H 5),\"\")]"
    it "e2" $
      show (analiza expr3 "2*(3+5)")
      `shouldBe` "[(N '*' (H 2) (N '+' (H 3) (H 5)),\"\")]"
    it "e3" $
      show (analiza expr3 "2+3*5")
      `shouldBe` "[(N '+' (H 2) (N '*' (H 3) (H 5)),\"\")]"
    it "e4" $
      show (analiza expr3 "2*3+5abc")
      `shouldBe` "[(N '+' (N '*' (H 2) (H 3)) (H 5),\"abc\")]"

  describe "arbolAnalisis" $ do
    it "e1" $
      show (arbolAnalisis "2*3+5")
      `shouldBe` "N '+' (N '*' (H 2) (H 3)) (H 5)"
    it "e2" $
      show (arbolAnalisis "2*(3+5)")
      `shouldBe` "N '*' (H 2) (N '+' (H 3) (H 5))"
    it "e3" $
      show (arbolAnalisis "2 * 3 + 5")
      `shouldBe` "N '+' (N '*' (H 2) (H 3)) (H 5)"
    it "e4" $
      evaluate (arbolAnalisis "2*3x+5y")
      `shouldThrow` errorCall "entrada sin usar x+5y"
    it "e5" $
      evaluate (arbolAnalisis "-1")
      `shouldThrow` errorCall "entrada no valida"

  describe "listaNV" $ do
    it "e1" $
      analiza (listaNV natural) "[3, 5,4]"
      `shouldBe` [([3,5,4],"")]
    it "e2" $
      analiza (listaNV natural) "[3, 5,4.0]"
      `shouldBe` []
    it "e3" $
      analiza (listaNV identificador) "[hoy , es,lunes ]"
      `shouldBe` [(["hoy","es","lunes"],"")]
    it "e4" $
      analiza (listaNV identificador) "[hoy , es,lunes,18 ]"
      `shouldBe` []

  describe "exprPBA" $ do
    it "e1" $
      analiza exprPBA "(())()"  `shouldBe`  [((),"")]
    it "e2" $
      analiza exprPBA "(()))("  `shouldBe`  [((),")(")]

  describe "exprPBA2" $ do
    it "e1" $
      analiza exprPBA2 ""          `shouldBe`  [(0,"")]
    it "e2" $
      analiza exprPBA2 "()"        `shouldBe`  [(1,"")]
    it "e3" $
      analiza exprPBA2 "()()"      `shouldBe`  [(1,"")]
    it "e4" $
      analiza exprPBA2 "(())()"    `shouldBe`  [(2,"")]
    it "e5" $
      analiza exprPBA2 "((())())"  `shouldBe`  [(3,"")]
    it "e6" $
      analiza exprPBA2 "())("      `shouldBe`  [(1,")(")]
