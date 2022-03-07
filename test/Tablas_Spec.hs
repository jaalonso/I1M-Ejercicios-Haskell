module Tablas_Spec ( spec, main ) where

import Tablas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inserta" $ do
    it "e1" $
      inserta 2 'a' vacia
      `shouldBe` Tabla [(2,'a')]
    it "e2" $
      inserta 4 'd' (inserta 2 'a' vacia)
      `shouldBe` Tabla [(4,'d'),(2,'a')]
  describe "borra" $ do
    it "e1" $
      borra 2 (Tabla [(2,'a'),(3,'b'),(2,'a')])
      `shouldBe` Tabla [(3,'b')]
  describe "busca" $ do
    it "e1" $
      busca 2 (Tabla [(2,'a'),(3,'b'),(2,'d')])  `shouldBe`  Just 'a'
    it "e1" $
      busca 4 (Tabla [(2,'a'),(3,'b'),(2,'d')])  `shouldBe`  Nothing
  describe "aplicaValores" $ do
    it "e1" $
      aplicaValores (+2) (Tabla [('a',5),('b',7),('c',4)])
      `shouldBe` Tabla [('a',7),('b',9),('c',6)]
  describe "aplicaClaves" $
    it "e1" $
      aplicaClaves (+2) (Tabla [(2,'a'),(3,'b'),(2,'d')])
      `shouldBe` Tabla [(4,'a'),(5,'b'),(4,'d')]
  describe "ajusta" $ do
    it "e1" $
      ajusta (\_ -> Nothing) 4 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2)]
    it "e2" $
      ajusta (\_ -> Nothing) 7 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2),(4,5)]
    it "e3" $
      ajusta (\_ -> Just 9) 4 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2),(4,9)]
    it "e4" $
      ajusta (\_ -> Just 9) 7 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2),(4,5),(7,9)]
    it "e5" $
      ajusta ((+ 2) <$>) 4 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2),(4,7)]
    it "e6" $
      ajusta ((+ 2) <$>) 7 (Tabla [(3,2),(4,5)])
      `shouldBe` Tabla [(3,2),(4,5)]
    it "e7" $
      ajusta (\_ -> Nothing) 3 (Tabla [])
      `shouldBe` (Tabla [] :: Tabla Int Char)
    it "e8" $
      ajusta (\_ -> Just 7) 3 (Tabla [])
      `shouldBe` Tabla [(3,7)]
    it "e9" $
      ajusta (\_ -> Nothing) 3 (Tabla [(3,1),(2,5),(3,7),(4,3)])
      `shouldBe` Tabla [(2,5),(4,3)]
    it "e10" $
      ajusta ((+ 2) <$>) 3 (Tabla [(3,1),(2,5),(3,7),(4,3)])
      `shouldBe` Tabla [(3,3),(2,5),(3,7),(4,3)]
    it "e11" $
      ajusta (\_ -> Nothing) 3 (Tabla [(2,5),(3,7),(4,3)])
      `shouldBe` Tabla [(2,5),(4,3)]
    it "e12" $
      ajusta ((+ 2) <$>) 3 (Tabla [(2,5),(3,7),(4,3)])
      `shouldBe` Tabla [(2,5),(3,9),(4,3)]
