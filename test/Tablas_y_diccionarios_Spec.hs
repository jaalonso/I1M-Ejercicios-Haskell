module Tablas_y_diccionarios_Spec ( spec, main ) where

import Tablas
import Tablas_y_diccionarios
import Data.Map.Strict
import Prelude
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inserta" $
    it "p1" $
      property prop_inserta
  describe "tablaAdiccionario" $
    it "e1" $
      tablaAdiccionario (inserta 4 'd' (inserta 2 'a' vacia))
      `shouldBe` fromList [(2,'a'),(4,'d')]
  describe "borra" $
    it "p1" $
      property prop_borra
  describe "busca" $
    it "p1" $
      property prop_busca
  describe "aplicaValores" $
    it "p1" $
      property prop_aplicaValores
  describe "aplicaClaves" $
    it "p1" $
      property prop_aplicaClaves
  describe "ajusta" $
    it "p1" $
      property prop_ajusta
